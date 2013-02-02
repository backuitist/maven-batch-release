import scala.sys.process.Process
import scala.xml.Elem;
import xml.XML
import java.io.File

object MavenTools {

	lazy val projectRootFolder = findProjectRootFolder()
	lazy val localArtifacts = new LocalArtifacts( getLocalArtifactsFromFolder( projectRootFolder ) )

	case class Version( val version : String ) extends Ordered[Version] {
		val versionNoSnapshot = version.replaceAll( "-SNAPSHOT", "" )
		val versions = versionNoSnapshot.split( "\\." )

		// we might want to support version numbers with more than 3 parts ?
		if( versions.size < 2 || versions.size > 3 )
			throw new IllegalArgumentException( "Incorrect version " + version + ", must have 2 to 3 parts separated with '.'," + 
				" but found " + versions.size + " parts" )

		val isSnapshot = version.endsWith( "SNAPSHOT" )

		private def parse( compound : String ) = Integer.parseInt( compound )

		val high = parse( versions( 0 ) )
		val middle = parse( versions( 1 ) )
		val small = if( versions.size > 2 ) parse( versions( 2 ) ) else 0

		def compare(other : Version) = {
			if( version == other.version ) 0
			else if( high != other.high ) high.compare( other.high )
			else if( middle != other.middle ) middle.compare( other.middle )
			else if( small != other.small ) small.compare( other.small )
			else if( isSnapshot ) -1 else 1 // can't be 0 since version != other.version
		}		
	}

	object Version {
		def apply( artifact : Artifact ) : Version = Version( artifact.version )

		def apply( localArtifact : LocalArtifact ) : Version = Version( localArtifact.artifact.version )
	}

	case class Artifact( id : String, groupId : String, version : String ) {
		val isSnapshot = version.endsWith("SNAPSHOT")
	}

	case class LocalArtifact( path : String, artifact : Artifact ) 

	class LocalArtifacts( val artifacts : List[LocalArtifact] ) {
		val localMap = Map( artifacts.map( la => la.artifact -> la ) : _* )

		def getLocalArtifactForArtifact( artifact : Artifact ) = localMap.get( artifact )

		def getLocalArtifactForArtifact( artifactId : String, groupId : String ) : Option[LocalArtifact] = 
			artifacts.find( la => la.artifact.id == artifactId && la.artifact.groupId == groupId )

		def getPathForArtifact( artifact : Artifact ) = localMap.get( artifact ).map( _.path )

		def getPathForArtifactId( artifactId : String ) = artifacts.find( _.artifact.id == artifactId ).map( _.path )
	}

	case class Dependency( artifact : Artifact, depType : String, classifier : String )

	def findProjectRootFolder() : String = {
		var testPath = ""
		var pathToReturn = ""

		def pomExists = new File( testPath + "pom.xml" ).exists

		while( pomExists ) {
			pathToReturn = testPath
			testPath = testPath + "../"
		}

		if( testPath.isEmpty ) {
			throw new IllegalStateException( "The current folder isn't a maven project !" )
		}

		pathToReturn
	}

	implicit def nodeSeqToString( ns : scala.xml.NodeSeq ) = ns.text 

	def getLocalArtifactsFromFolder( path : String ) :List[LocalArtifact] = {

		val pom = xml.XML.loadFile(path + "/pom.xml" )

		val packaging : String = pom \ "packaging"
		val artifactId = pom \ "artifactId"

		val rootGroupId = pom \ "groupId"
		val parentGroupId = pom \ "parent" \ "groupId"
		val groupId = if( rootGroupId.isEmpty ) parentGroupId else rootGroupId

		val rootVersion = pom \ "version"
		val parentVersion = pom \ "parent" \ "version"
		val version = if( rootVersion.isEmpty ) parentVersion else rootVersion
	
		val localArtifact = LocalArtifact( path, Artifact( artifactId, groupId, version ) ) 

		localArtifact :: (
			if( packaging == "pom" ) {

				val modules =  pom \ "modules" \ "module"

				(for( moduleXml <- modules ) yield {
					val module = moduleXml.text
					getLocalArtifactsFromFolder( path + "/" + module )
				}).flatten.asInstanceOf[List[LocalArtifact]]
			} else {
				Nil
			}
		)
	}

	def getDependenciesFromPom(pom : Elem ) : List[Dependency] = {
		val dependencies = pom \ "dependencies" \ "dependency"
		(for( dependency <- dependencies ) yield {
			Dependency( Artifact( 
					dependency \ "artifactId",
					dependency \ "groupId",
					dependency \ "version" ),
				dependency \ "type",
				dependency \ "classifier" )
		}).asInstanceOf[List[Dependency]]
	}

	def getDependencies(path : String) : List[Dependency] =
		getDependenciesFromPom( XML.loadFile( path + "/pom.xml" ) )

	def getDependenciesFromPom(pom : String ) : List[Dependency] = 
		getDependenciesFromPom( XML.loadString( pom ) )
	
	def getDependencies(localArtifact : LocalArtifact ) : List[Dependency] =
		getDependencies(localArtifact.path)

	def effectivePom() : String = {
		val effectivePomProcess = Process( "mvn help:effective-pom" )
		val lines = effectivePomProcess.lines
	
		// remove all lines starting with "[INFO]" and the one line that says 
		// "Effective POMs, after inheritance, interpolation, and profiles are applied"
		// then merge the lines into one string
		lines.filter( l => ! l.startsWith( "[INFO]" ) ).splitAt( 1 )._2.reduce( _ + _ )
	}	
}
