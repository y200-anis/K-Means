import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal

/* Auteurs :	 WENGER Marlon
* 						AIT TAOUIT Yanis
*
* Groupe :	 MA 									*/

object KMeans :

	/* 	Changer le filename par le path du ficher .data pour tester le kmeans
	* ATTENTION : Il faut retirer tout ligne vide ou retour à la ligne inutile
	* Mettre le chemin absolu si le chemin relatif ne marche pas (dans filename) */
	//val filename = "C:\\Users\\tehre\\IdeaProjects\\test\\src\\main\\scala\\iris.data"
	val filename = "iris.data"
	val NbLignes = io.Source.fromFile(filename).getLines.size


	/* ****************************************************************************
	******************                MAIN              ***************************
	**************************************************************************** */


	def main(args: Array[String]) : Unit = {

		val Mat = ArrayBuffer[Array[String]]()
		for (line <- Source.fromFile(filename).getLines) {
			Mat += line.split(",")
			}
		for(line <- Mat) {
			println(line.mkString(" "))
			}

		println("Nb colonnes : " + (Mat(0).length-1))

		/*
		println("Moyenne " + Moyenne(Mat).mkString(" "))
		println("Variance : " + Variance(Mat,Moyenne(Mat)).mkString(" "))
		println("Ecart type : " + EcartType(Variance(Mat,Moyenne(Mat))).mkString(" "))
		println("Distance entre les deux premiers points : " + Mat(0).mkString(",") + " et " + Mat(1).mkString(",") + " " + dist(Mat(0),Mat(1)))

		*/
		println("###############  Cluster initiaux :  ############### \n")
		var ini : Array[ArrayBuffer[Array[String]]] = InitClusters(3,Mat)
		afficheClusters(ini)

		println("############### Centroides des clusters générés : ############### \n")
		var listecentroideini = CalculerListeCentroides(ini)
		afficherListeCentroides(listecentroideini)

		println("############### Mise à jour des clusters initiaux ############### \n")
		var clusterMAJ = MiseAJourCluster(ini,listecentroideini)
		var listCent = CalculerListeCentroides(clusterMAJ)

		var iteration = 1

		while(estDifferent(listCent,listecentroideini)) {
			println("########################## ITERATION N°"+iteration+" ########################## \n")

			println("\n")
			println("\n")

			println("############### LISTE CENTROIDES INITIAUX ###############")
			afficherListeCentroides(listecentroideini)

			println("############### LISTE DES NOUVEAUX CENTROIDES ###############")
			afficherListeCentroides(listCent)

			println("############### MISE A JOUR CLUSTERS ###############")
			println("############### NOUVEAUX CLUSTERS : ###############")
			afficheClusters(clusterMAJ)
			listecentroideini = listCent
			ini = clusterMAJ
			clusterMAJ = MiseAJourCluster(clusterMAJ,listCent)
			listCent = CalculerListeCentroides(clusterMAJ)
			iteration +=1
		}

		println("\n")
		println("\n")
		println("########################## Résultat final ! ##########################")

		afficheClusters(clusterMAJ)
		println('\n')
		println("Liste des centroides finaux :")

		afficherListeCentroides(listCent)
		println("\n")
		}

	/* ****************************************************************************
	******************         FIN DU MAIN              ***************************
	**************************************************************************** */



		def Moyenne(Mat:ArrayBuffer[Array[String]]) : Array[Double] = {
			/* Moyenne : Prend en paramètre un cluster ou une liste de points et retourne un liste des moyennes de chaque data (une moyenne pour petal.witdh,etc ..), donc chaque colonne
			* Dans notre cas, elle retournera une liste de 4 moyennes */
			if(Mat(0).length == 0) return Array(0.0,0.0,0.0,0.0)	// sécurité au cas où le cluster passé est vide
			val tab: Array[Double] = new Array[Double](Mat(0).length-1)
			for(line <- Mat) {
				for (j <- tab.indices) do {
					tab(j) = tab(j) + line(j).toDouble
					}
				}
				for (i <- tab.indices) do tab(i) /= Mat.length
			tab
			}
			
		def Variance(Mat:ArrayBuffer[Array[String]],Moy:Array[Double]) : Array[Double] = {
			val tab: Array[Double] = new Array[Double](Moy.length)
			for(line <- Mat) {
				for(j <- tab.indices) {
					tab(j) += ((line(j).toDouble - Moy(j)) * (line(j).toDouble - Moy(j)))
				}

				/*tab(0) += ((line(0).toDouble - Moy(0)) * (line(0).toDouble - Moy(0)))
				tab(1) += (line(1).toDouble - Moy(1) * (line(1).toDouble - Moy(1)))
				tab(2) += (line(2).toDouble - Moy(2) * (line(2).toDouble - Moy(2)))
				tab(3) += (line(3).toDouble - Moy(3) * (line(3).toDouble - Moy(3)))*/
				}
				for (i <- tab.indices) tab(i) /= Mat.length
			tab
			}

		def EcartType(Variance:Array[Double]) : Array[Double] = {
			val tab = new Array[Double](Variance.length)
			for(i <- Variance.indices){
				tab(i) = scala.math.sqrt(Variance(i))
				}
			tab
			}


	def estDifferent(ListeCentroides1 : Array[Array[String]] , ListeCentroides2: Array[Array[String]] ) : Boolean = {
		/* Fonction estDifferent : Prend en paramètre deux Listes de Centroides et renvoie true si elle sont différentes
		* false sinon */

		for(i <- ListeCentroides1.indices) {
			for (j <- ListeCentroides1(i).indices) do if ListeCentroides1(i)(j) != ListeCentroides2(i)(j) then return true
		}

		false
	}

	def afficherListeCentroides(listeCentroides : Array[Array[String]]) : Unit = {
			//println("------------  Liste des Centroides ----------------")
			for (e <- listeCentroides) do println(e.mkString(" "))
		}
		def dist(x:Array[String],y:Array[String]) : Double = {
			/* Fonction dist : Prend en paramètre deux points et calcule leur distance euclidienne */

			var sum : Double = 0
			for(i <- 0 until x.length-1){
				sum += ((x(i).toDouble - y(i).toDouble) * (x(i).toDouble - y(i).toDouble))
				}
			scala.math.sqrt(sum)
			}
		
		def InitClusters(K:Int, Mat:ArrayBuffer[Array[String]]) : Array[ArrayBuffer[Array[String]]] = {
			/* Fonction InitClusters : Prend en paramètre un nombre de clusters K, La matrice contenant les info du fichiers : Mat
			* et retourne la liste des K clusters initialisés aléatoirement */

			val Mat2 = Mat
			val r = scala.util.Random
			val ListeClusters: Array[ArrayBuffer[Array[String]]] = new Array[ArrayBuffer[Array[String]]](K) // Initialisation de la liste à retourner (qui va contenir les clusters)

			val tailleCluster = NbLignes / K																					// Nblignes contient le nombre de lignes du fichier (ou Mat.length), ici 150 car 150 points
			for(i <- 0 until K){ 																											// On itère K fois pour créer K clusters
				val tmp = new ArrayBuffer[Array[String]]() // A chaque itération on crée un cluster (tmp)
				for(j <- 0 until tailleCluster){																				// On itère tailleCluster fois (ici 50 fois)
					val indexrandom = r.nextInt(Mat2.length) // On choisit un index aléatoire pour le prendre dans Mat
					tmp += Mat2(indexrandom)																							// On ajoute ce point aléatoire dans le cluster (+= équivaut au .add() pour les ArrayBuffers
					Mat2 -= Mat2(indexrandom)																							// On le retire de Mat pour pas retomber dessus par la suite
					}

				ListeClusters(i) = tmp																									// Une fois le cluster rempli, on le met dans la liste à retourner
				}

			ListeClusters
			}

		def afficheClusters(ListeClusters : Array[ArrayBuffer[Array[String]]] ) : Unit = {
			var ind = 1
			for(i <- ListeClusters) {
				println("------------ CLUSTER " + "#" + ind + " ---------------")
				println("Taille du cluster : " + i.length)
				for (j <- i ){
					println(j.mkString(" "))
				}
				ind+=1
			}
		}

		def CalculerCentroide(Cluster:ArrayBuffer[Array[String]]): Array[String] = {
			/* CalculerCentroide : Prend en paramètre un Cluster et retoune son centroide */
			val moyenne = Moyenne(Cluster) 							// Liste de des moyennes pour le cluster en paramètre (ici 4 moyennes)
			val res = new Array[String](moyenne.length)
			for (i <- moyenne.indices) do res(i) = BigDecimal(moyenne(i)).setScale(1,BigDecimal.RoundingMode.HALF_UP).toDouble.toString // Longue syntaxe qui permet d'arrondir au premier
			res																																																												// chiffre après la virgule
		}

		def CalculerListeCentroides(ListeClusters : Array[ArrayBuffer[Array[String]]]): Array[Array[String]] ={
			/* CalculerListeCentroides : Prend en paramètre une liste de K clusters et calcule les K centroides et les retourne dans une liste
			* Il s'agit juste d'appeler la fonction CalculerCentroide pour chaque cluster et mettre chaque résultat dans une liste*/

			val ListeCentr = new Array[Array[String]](ListeClusters.length)
			for (i <- ListeClusters.indices) do {
				ListeCentr(i) = CalculerCentroide(ListeClusters(i))
			}
			ListeCentr
		}

		def IndexCentroidePlusProche(ListeCentroide : Array[Array[String]], point : Array[String]) : Int = {
			/* IndexCentroidePlusProche : Prend en paramètre une liste de centroides et un point et retourne l'index du centroide le plus proche
			* Par exemple si le centroide le plus proche est en index 1 dans la liste, la fonction retourne 1*/

			var IndexMini = 0
			var DistanceMini = dist(ListeCentroide(0),point)
			for(i <- 1 until ListeCentroide.length){
				val tmp = dist(ListeCentroide(i), point)
				if (tmp < DistanceMini) {
					IndexMini = i
					DistanceMini = tmp
				}
			}

			IndexMini
		}

		def MiseAJourCluster(ListeClusters : Array[ArrayBuffer[Array[String]]], ListeCentroides : Array[Array[String]]) : Array[ArrayBuffer[Array[String]]] ={
			/* MiseAJourCluster : Prend en paramètre une liste de Clusters et une liste de centroides et renvoie la liste des clusters mis à jours */

			//var ListeCentroides = CalculerListeCentroides(ListeClusters)
			val NouvelleListeCluster = new Array[ArrayBuffer[Array[String]]](ListeClusters.length)								//Initialisation de la liste contenant les clusters (à retourner)
			for (i <- NouvelleListeCluster.indices) do NouvelleListeCluster(i) = new ArrayBuffer[Array[String]]() //Initialisation de chaque cluster
			for(Cluster <- ListeClusters) {															// On itère sur chaque cluster
				for(i <- 0 until Cluster.length) {                        // On itère sur chaque point du cluster
					/* On sait que si c'est le centroide 1  le plus proche alors le point doit aller dans le cluster 1
					* Pour rappel ListeCentroides contient une liste avec les centroides, ils partagent le même index que le cluster auquel ils sont associés
					* ex : le centroide d'index 1 dans ListeCentroides est le centroide correspondant au cluster qui est aussi d'indice 1 (qui lui est dans ListeClusters)*/
					val IndexCentroide : Int = IndexCentroidePlusProche(ListeCentroides,Cluster(i)) // On calcule quel est le centroide le plus proche (sa position dans ListeClusters)
					NouvelleListeCluster(IndexCentroide) += Cluster(i)        // On ajoute le point dans la nouvelle liste de clusters à l'indice du centroide le plus proche dans liste cluster
																																		// Il sera donc ajouté au cluster avec le centroide correspondant
				}
			}

			NouvelleListeCluster
		}
		
