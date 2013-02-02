import MavenTools._
import java.io.File

abstract class ReleaseViolation {
	def fix : ViolationFix
}

abstract class ViolationFix {
	def isPossible : Boolean
	def description : String
}

case class ViolationNoFix extends ViolationFix {
	val isPossible = false;
	val description = "";
}

case class UpgradeVersionViolationFix( fromVersion : String, toVersion : String ) extends ViolationFix {
	val isPossible = true;
	val description = "The dependency can be upgrade from " + fromVersion + " to " + toVersion;
}

class DependencyPath( val path : List[Dependency] ) {
	def ->( dep : Dependency ) = new DependencyPath( dep :: path )
	
	override lazy val toString = path.reverse.mkString( " -> " )
}

object DependencyPath {
	def apply() = new DependencyPath( Nil )
}

case class IncorrectVersionViolation( requiredVersion : Artifact, pathToDependency : DependencyPath, availableVersion : LocalArtifact ) extends ReleaseViolation {
	val fix : ViolationFix = try {
		if( Version( availableVersion ) > Version( requiredVersion ) ) {
			UpgradeVersionViolationFix( requiredVersion.version, availableVersion.artifact.version )
		} else {
			ViolationNoFix
		}
	} catch {
		case _ => ViolationNoFix
	} 
}

case class ExternalArtifactViolation( artifact : Artifact, pathToDependency : DependencyPath ) extends ReleaseViolation {
	val fix = ViolationNoFix()
}

class BatchRelease( val artifacts : List[LocalArtifact], val violations : List[ReleaseViolation] ) {
	/** no violations & nothing to release */
	lazy val noOp = artifacts.isEmpty && violations.isEmpty

	lazy val hasViolations = ! violations.isEmpty

	lazy val canFixAllViolations = violationsWithFix.size == violations.size

	lazy val violationsWithFix = violations.filter( _.fix.isPossible )

	def formatViolations = "* " + violations.mkString( "\n* " )

	override def toString() = "BatchRelease[ " + artifacts + ", " + violations + " ]";
}


object BatchRelease {

	def apply() : BatchRelease = {
		val projectRootFolder = findProjectRootFolder()
		val localArtifacts = new LocalArtifacts( getLocalArtifactsFromFolder( projectRootFolder ) )

		println( "Found " + localArtifacts.artifacts.size + " artifacts in current project (root located at " + projectRootFolder + ")" )

		val dependencies = getDependenciesFromPom( effectivePom() )		
		val snapshots = dependencies.filter( _.artifact.isSnapshot )
		println( "Found " + dependencies.size + " dependencies of which " + snapshots.size + " are SNAPSHOTs" )

		apply( localArtifacts, dependencies )
	}

	def apply( localArtifacts : LocalArtifacts, dependencies : List[Dependency] ) : BatchRelease =
		apply( localArtifacts, DependencyPath(), dependencies, Nil, Nil )

	private def apply( localArtifacts : LocalArtifacts, pathToDependency : DependencyPath, dependencies : List[Dependency], toBeReleased : List[LocalArtifact], violations : List[ReleaseViolation] ) : BatchRelease = {
		dependencies match {
			case Nil => new BatchRelease(toBeReleased,violations)
			case dependency :: deps =>
		
				/** skip dependency, process deps **/
				def skip = apply( localArtifacts, pathToDependency, deps, toBeReleased, violations )

				val artifact = dependency.artifact

				if( artifact.isSnapshot ) {			
					localArtifacts.getLocalArtifactForArtifact( artifact ) match {
						case None => 
							val violation = 							
								// check for superior version
								localArtifacts.getLocalArtifactForArtifact( artifact.id, artifact.groupId ) match {
									case None => ExternalArtifactViolation( artifact, pathToDependency )
									case Some( matchingIdGroupId ) => IncorrectVersionViolation( artifact, pathToDependency, matchingIdGroupId )
								}

							apply( localArtifacts, pathToDependency, deps, toBeReleased, violation :: violations )

						case Some( localArtifact ) =>
							if( toBeReleased.contains( localArtifact ) ) {
								// this dependency has already been seen through previous dependencies
								skip
							} else {
								// batch release the dependencies of the dependency
								val release = apply( localArtifacts, pathToDependency -> dependency,
									getDependencies( localArtifact ), toBeReleased, violations )					

								// add the current dependency and process the remaining dependencies
								apply( localArtifacts, pathToDependency, deps, localArtifact :: release.artifacts, release.violations )
							}
					}
				} else {
					// not a snapshot
					skip
				}
		}	
	}


	def main( args : Array[String]) {
		val release = BatchRelease()
		if( release.hasViolations ) {
			if( release.canFixAllViolations ) {
				println( "The release cannot be performed as such, but some actions can be taken in order to solve these issues : " )
				for( violation <- release.violationsWithFix ) {
					println( " -> " + violation.fix.description )				
				}
			}			

			println( "Cannot release due to the following " + release.violations.size + " violations :" )
			println( release.formatViolations )
		} else {
			println( "Releasing :" )
			println( "* " + release.artifacts.mkString( "\n* " ) )
		}
	}
}
