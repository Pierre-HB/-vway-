"présentation";
	"
	J'ai résolue le problème rencontré lors de l'execution du jeu sur les ordinateur du lycée, je n'ai cependant pas eu l'occasion de tester a nouveau le jeu. Je ne sais donc pas s'il fonction sur un autre ordinateur que le mien :( )
	";;
	"Aprés avoir implémenter un arbre BSP pour les rendu graphique, le jeu a cesser de fonctionner, je n'ai pas pus trouver la cause de l'erreur. Ceci est donc une version du jeu qui utilise l'allgorithme du paintre et non les arbres BSP pour le rendu visuel.
	";;
	"
	Le but de ce jeu est de sauver la planète Kïn, en détruisant le Destroyer Imperial.
	Pour accélérer il faut appuyer sur la barre d'espace, pour tirer il faut cliquer avec la sourceInCameraRepere
	pour s'orienter il suffit de lever le curseur:
	-verticalement pour lever ou baisser la tete
	-horizontalement pour 'courber la tete'

	(une partie moyenne dure 1 minute, une partie accomplie dure deux minutes, les plus longues peuvent aller jusqu'à 5 minutes
	";;

	"fonctionnement général du jeu (algorithmiquement parlant):
	le jeu est simplement une liste d'entités qui vont tour a tour s'actualiser (les fonctions nomées update)
	et s'afficher (les fonctions nomées render)
	";;

	"
	Les calculs 3D utilisant des matrices, voici quelques notations que j'emploierai lors des expliquations théoriques:
	Pour a et b deux bases de l'espace euclidien, on notera:
	B(a->b): la matrice de passage de a vers b
	bc: la base canonique
	b-cam: la base dans laquelle la caméra est à l'origine, pointant vers l'axe (1, 0, 0)
	b-obj: la base dans laquelle les .obj sont codés (la base relative à chaque objet, il y a donc plusieur bases de ce type, mais elle seront toutes représentées par ce même symbole pour des questions de lisibilité)
	";;

#load "Unix.cma" ;;
#load "Graphics.cma";;
#load "Str.cma";;

let identifier = ref 0;;
let get_id () = 
	incr identifier;
	!identifier;;

let foreach_list liste callback = 
	(*applique la une fonction à tout les element d'une list*)
	let temp = ref liste in
	while !temp <> [] do
		let h::t = !temp in
		temp := t;
		callback h
	done;;

let foreach_array tableau callback = 
	(*applique une fonction à chaque element d'une array*)
	for i = 0 to Array.length tableau - 1 do
		callback tableau.(i)
	done;;

type phisic_data = {minSpeed : float; maxSpeed : float; mutable radius : float; motorPropulsion : float; frottement : float; identifiant : int};;
type waveContent = Hunter | Bombarder;;
"Constantes";;
	let ti = int_of_float and
		tf = float_of_int and
		pi = acos (-.1.) and
		dt = 0.002 and
		tick = 120 and 
		fps = 120;;

	let frecUpdate = 1. /. tf tick and
		frecFps = 1. /. tf fps;;

	let starPerScreen = 600 and
		nbStars = 600 and
		delayBetweenLetters = 16 and
		timePerLetter = 12;;

	let explosionAttaque = 2;;

	let planetteRotationSpeed = 0.0002 and
		planetteRadius = 100. and
		planetteHP = 3000 and
		planetteAttack = 100;;

	let sunRotationSpeed = 0.00002 and
		sunRadius = 500. and
		sunHP = max_int and
		sunAttack = max_int;;
		
	let iconeSize = 0.8 and
		iconeMin = 10. and
		iconeMax = 50. and
		iconsPerScreen = 50 and
		iconeColorFullLife = Graphics.rgb 255 0 0 and
		iconeColorMidLife = Graphics.rgb 200 50 50 and
		iconeColorNoLife = Graphics.rgb 180 60 100;;

	let turistReloadTime = 15*4 and
		surcaultCouldown = 1*4 and
		surcaulImpact = 0.00003 and
		turistAttack = 50 and
		turistHP = 500 and
		playerReflexe = 4 and
		turisteSensibilite = 0.2/.4.;;

	let hunterMinSpeed = 10. and
		hunterMaxSpeed = 100. and
		hunterMotorPower = 10. and
		hunterReloadTime = 30*4 and
		hunterRadius = 3. and
		hunterAttack = 50 and
		hunterShootDistance = 60. and
		hunterAccelerateDistance = 40. and
		hunterIconeRadius = 50. and
		hunterHP = 100 and
		hunterSensibilite = 0.1/.4.;;

	let bombarderRadius = 3. and
		bombarderAttack = 50 and
		bombeSpeed = 15. and
		bombarderShootDistance = 60. and
		bombarderAccelerateDistance = 40. and
		bombarderReloadTime = 300*4 and
		bombarderIconeRadius = 50. and
		bombarderHP = 100 and
		bombarderSensibilite = 0.1/.4.;;

	let destroyerRadius = 50.;;
	let destroyerAttack = 50 and
		destroyerShootDistance = planetteRadius*.1.2 +. destroyerRadius and
		destroyerAccelerateDistance = planetteRadius*.1.2 +. destroyerRadius and
		destroyerReloadTime = 300*4 and
		destroyerIconeRadius = 50. and
		destroyerHP = 2000 and
		destroyerSensibilite = 0.01/.4.;;

	let bombeTimeLimit = 300*4 and
		bombeAttack = 100 and
		bombeIconeRadius = 50. and
		bombeMinSpeed = 60. and
		bombeMaxSpeed = 60. and
		bombeRadius = 2. and
		bombeHP = 100 and
		bombeSensibilite = 0.5/.4.;;

	let laserSpeed = 300. and
		laserTimeLimit = 90*4 and
		laserRadius = 1.;;

	(*l'ordre d'aparition des differents vaisseau ennemies*)
	let waves = [|(Hunter, 0.15);(Hunter, 0.1);(Bombarder, 0.15);(Hunter, 0.1);(Bombarder, 0.1);(Bombarder, 0.15);(Hunter, 0.1);(Hunter, 0.1);(Hunter, 0.1)|];;	

	(*les caractéristique des differentes entités*)
	let get_turist_phiscal_data () = {minSpeed = 40.; maxSpeed = 100.; radius = 1.; motorPropulsion = 5.; frottement = 0.95; identifiant = get_id ()};;
	let get_hunter_phiscal_data () = {minSpeed = 0.; maxSpeed = 70.; radius = hunterRadius; motorPropulsion = 1.; frottement = 0.95; identifiant = get_id ()};;
	let get_bombarder_phiscal_data () = {minSpeed = 0.; maxSpeed = 70.; radius = bombarderRadius; motorPropulsion = 0.7; frottement = 0.95; identifiant = get_id ()};;
	let get_destroyer_phiscal_data () = {minSpeed = 0.; maxSpeed = 30.; radius = destroyerRadius; motorPropulsion = 0.5; frottement = 0.95; identifiant = get_id ()};;

"Mathématiques Matrix";;
	(*Les matrices mathématiques*)
	class matrix coefs = object (self)
		val l = Array.length coefs
		val c = Array.length coefs.(0)
		val coefs = coefs

		method dot coef = 
			for i = 0 to l-1 do
				for j = 0 to c-1 do
					coefs.(i).(j) <- coef *. coefs.(i).(j)
				done
			done
		method get_shape () = l, c
		method get_coef i j = coefs.(i).(j)
		method get_coefs () = coefs
	end;;
	(*Le produit matricielle*)
	let matrix_product m1 m2 = 
		let l1, c1 = m1#get_shape () and
			l2, c2 = m2#get_shape () in
		if c1 <> l2 then failwith "Les matrice ne corresponde pas";
		let m = Array.make_matrix l1 c2 0. in
		for i = 0 to l1-1 do
			for j = 0 to c2-1 do
				for k = 0 to c1-1 do
					m.(i).(j) <- m1#get_coef i k *. m2#get_coef k j +. m.(i).(j)
				done
			done
		done;
		new matrix m;;
	(*Donne la transposé d'une matrice*)
	let transpose_matrix mat = 
		let l, c = mat#get_shape () in 
		let transpose = Array.make_matrix c l 0. in
		for x = 0 to l - 1 do
			for y = 0 to c - 1 do
				transpose.(y).(x) <- mat#get_coef x y
			done
		done;
		new matrix transpose;;
	(*permet de faire un addition ou une soustraction dans Mn(R)*)
	let matrix_sum opp m1 m2 = 
		let l1, c1 = m1#get_shape () and
			l2, c2 = m2#get_shape () in
		if (l1, c1) <> (l2, c2) then failwith "les matrices n'ont pas les même dimension";
		let m = Array.make_matrix l1 c1 0. in
		for i = 0 to l1-1 do
			for j = 0 to c2-1 do
				m.(i).(j) <- opp (m1#get_coef i j) (m2#get_coef i j)
			done
		done;
		new matrix m;;
	let matrix_add m1 m2 = matrix_sum (+.) m1 m2;;
	let matrix_sub m1 m2 = matrix_sum (-.) m1 m2;;
	(*les coeffs sont donnés ligne par ligne dans une array à une dimension*)
	let create_matrix coefs shape = 
		let l, c = shape in
		let m = Array.make_matrix l c 0. in
		for i = 0 to l-1 do
			for j = 0 to c-1 do
				m.(i).(j) <- coefs.(i*c+j)
			done
		done;
		new matrix m;;

"Mathématiques Vector";;
	(*les vecteur de l'espace euclidien*)
	class vector3D x y z = object (self)
	inherit matrix [|[|x|];[|y|];[|z|]|] as super
		val mutable norme = 0.
		val mutable haveNorme = false
		
		method dot a = (*multiplie la norme du vetceur par a*)
			super#dot a;
			if haveNorme then norme <- norme*.a
		method get_norme () = (*renvoie la norme, calculer de manière optimiser*)
			if not haveNorme then begin
				norme <- 0.;
				for i = 0 to 2 do
					norme <- norme+.(super#get_coef i 0) *. (super#get_coef i 0)
				done;
				norme <- norme**0.5;
				haveNorme <- true
			end;
			norme
		method normalize () = (*rend le vecteur unitaire*)
			super#dot (1./.(self#get_norme ()));
			norme <- 1.;
			haveNorme <- true
		method get_coord i = super#get_coef i 0
		method set_coord i x = 
			(super#get_coefs ()).(i).(0) <- x;
			haveNorme <- false
		method add_coord i x = 
			(super#get_coefs ()).(i).(0) <- (super#get_coef i 0) +. x;
			haveNorme <- false
		method get_x () = self#get_coef 0 0
		method get_y () = self#get_coef 1 0
		method get_z () = self#get_coef 2 0
		method set_x x  = self#set_coord 0 x
		method set_y y  = self#set_coord 1 y
		method set_z z  = self#set_coord 2 z
		method add_x x  = self#add_coord 0 x
		method add_y y  = self#add_coord 1 y
		method add_z z  = self#add_coord 2 z
		method copy3D () = 
			(*renvoie une copy de se vecteur*)
			new vector3D (self#get_x ()) (self#get_y ()) (self#get_z ())
	end;;

	let convert_vector3D vect = 
		(*converti des matrice (qui on les bonne dimension) en vecteur3D*)
		if vect#get_shape () <> (3, 1) then failwith "le vecteur n'est pas un vecteur 3D";
		new vector3D (vect#get_coef 0 0) (vect#get_coef 1 0) (vect#get_coef 2 0);;

	let get_nul_3D () = new vector3D 0. 0. 0.;;

	let scalar v1 v2 =
		(*produit scalaire*)
		let s = ref 0. in
		for i = 0 to 2 do
			s := !s +. (v1#get_coord i) *. (v2#get_coord i)
		done;
		!s;;

	let cross3D v1 v2 = 
		(*produit vectoriel*)
		let x1 = v1#get_x () and
			y1 = v1#get_y () and
			z1 = v1#get_z () and
			x2 = v2#get_x () and
			y2 = v2#get_y () and
			z2 = v2#get_z () in
		let x = y1*.z2 -. z1*.y2 and
			y = z1*.x2 -. x1*.z2 and
			z = x1*.y2 -. y1*.x2 in
		new vector3D x y z;;

	let vector_sum opp v1 v2 = 
		(*permet d'ajouter ou soustraire deux vecteurs*)
		let x1 = v1#get_x () and
			y1 = v1#get_y () and
			z1 = v1#get_z () and
			x2 = v2#get_x () and
			y2 = v2#get_y () and
			z2 = v2#get_z () in
		new vector3D (opp x1 x2) (opp y1 y2) (opp z1 z2);;

	let vector_add v1 v2 = vector_sum (+.) v1 v2;;
	let vector_sub v1 v2 = vector_sum (-.) v1 v2;;

let rec quick_sort tableau tri debut fin =
	(*trie une array des indice 'debut' a 'fin' a l'aide de la fonction 'tri'*)
	if fin - debut >=1 then begin
    	let pivot = tableau.(debut) in
    	let sup = ref (fin-1) and
    	inf = ref debut in
    	let temp = ref tableau.(!sup) in
    	for i = 0 to fin-debut-2 do
    	    if tri !temp pivot then begin
    	        tableau.(!inf) <- !temp;
    	        incr inf;
    	        temp := tableau.(!inf)
    	   	 	end
    	    else begin
    	        tableau.(!sup) <- !temp;
    	        decr sup;
    	        temp := tableau.(!sup)
    	    	end
    	    done;
    	tableau.(!inf) <- pivot;
    	if !inf - debut > 1 then quick_sort tableau tri debut !inf;
    	if fin - !sup > 1 then quick_sort tableau tri (!sup+1) fin
	end;;

"Types";;
	type movesMatrixs = {rotation : matrix; translation : vector3D};;
	type repere = {mutable e1 : vector3D; mutable e2 : vector3D; mutable e3 : vector3D; mutable o : vector3D};;
	type repereCinetique = {mutable rep : repere; mutable v : float};;
	type camera = {mutable size_x : int; mutable size_y : int; ouverture : float; mutable a : float; d : float; mutable ad : float; cosO : float};;
	type color = {mutable r : float; mutable g : float; mutable b : float};;(*les couleur de se type sont stoquer sous la forme des carré de leur valeur usuelle*)
	type materialType = Default|SunSurface|LaserMaterial|ExplosionMaterial|BombeMaterial|BMainMaterial|BLinkMaterial|BCokpitMaterial|BShieldMaterial|KinOcean|KinContinent|KinPole|DCanon|DCanonSupport|DMain|DCokpit|HMain|HArm|HLink|HCokpit;;
	type environnementLocal = { mutable lightDirection : vector3D; mutable lightColor : color};;
	type control = {pitch : float; roll : float; speedUp : bool; fire : bool};;
	type ship = Destroyer | Bombarder | Hunter | Turist;;

"Environnement et repère";;
	class environnement (source : vector3D) (lightColor : color) = object (self)
		(*permet de calculer la lumiere qui éclaire un objet en fonction de sa position*)
		val source = source (*position de la source lumineuse dans bc*)
		val lightColor = lightColor
		val mutable sourceInCameraRepere = get_nul_3D ()(*position de la source lumineuse dans b-cam*)

		method get_light_in_camera (position : vector3D) = 
			(*donne la direction de la lumière a une position de l'espace*)
			let l = vector_sub position sourceInCameraRepere in
			l#normalize ();
			l
		method move_light_in_camera (rotationMatrix : matrix) = 
			(*aplique B(bc->b-cam) a la position de la lumiére afin d'avoir les coordonné de cette dernière dans la b-cam*)
			sourceInCameraRepere <- convert_vector3D (matrix_product rotationMatrix source)
		method get_light_color () = lightColor
	end;;

	let create_color r g b = { r = tf (r*r); g = tf (g*g); b = tf (b*b)};;(*cree une couleur avec les valeurs usuel de la couleur*)
	
	let dot_color c coef = {r = c.r*.coef; g = c.g*.coef; b = c.b*.coef};;
	
	let copy_repere repere = (*copy un repere*)
		{e1 = repere.e1#copy3D (); e2 = repere.e2#copy3D (); e3 = repere.e3#copy3D (); o = repere.o#copy3D ()};;
	
	let copy_repereCinetique (repCin : repereCinetique) = (*copy un repere cinetique*)
		{rep = copy_repere repCin.rep; v = repCin.v};;

	let get_rotationMatrix ax ay az = (*donne la matrice de rotation pour les angle d'Euler: ax ay et az*)
		let rotationX = create_matrix [|1.;0.;0.; 0.;cos ax;-.sin ax; 0.;sin ax;cos ax;|] (3,3) and
			rotationY = create_matrix [|cos ay;0.;sin ay; 0.;1.;0.; -.sin ay;0.;cos ay;|] (3,3) and
			rotationZ = create_matrix [|cos az;-.sin az;0.; sin az;cos az;0.; 0.;0.;1.;|] (3,3) in
		matrix_product (matrix_product rotationZ rotationX) rotationY;;

	let get_moveMatrix position ax ay az sx sy sz = 
		(*renvoie B(bc->?) avec ? le repere hortogonal ayant subit la rotation*)
		(*ax ay az les angles d'Euler, sx sy sz les facteur de grossissement suivant les 3 axes*)
		let scale = create_matrix [|sx;0.;0.; 0.;sy;0.; 0.;0.;sz;|] (3,3) and
			rotation = get_rotationMatrix ax ay az in
		{rotation = (matrix_product rotation scale); translation = position#copy3D ()};;

	let get_changement_de_base_matrix repere = 
		(*matrice de changement de base: B(repere -> bc)*)
		let x1 = repere.e1#get_x () and
			y1 = repere.e1#get_y () and
			z1 = repere.e1#get_z () and
			x2 = repere.e2#get_x () and
			y2 = repere.e2#get_y () and
			z2 = repere.e2#get_z () and
			x3 = repere.e3#get_x () and
			y3 = repere.e3#get_y () and
			z3 = repere.e3#get_z () and
			translation = repere.o#copy3D () in
		{rotation = create_matrix [|x1;x2;x3; y1;y2;y3; z1;z2;z3|] (3,3); translation = translation};;

	let rotation_repere repere rotationMatrix = 
		(*applique une matrice de rotation a un repere*)
		let p = get_changement_de_base_matrix repere in
		let rot = matrix_product p.rotation rotationMatrix  in
		repere.e1 <- new vector3D (rot#get_coef 0 0) (rot#get_coef 1 0) (rot#get_coef 2 0);
		repere.e2 <- new vector3D (rot#get_coef 0 1) (rot#get_coef 1 1) (rot#get_coef 2 1);
		repere.e3 <- new vector3D (rot#get_coef 0 2) (rot#get_coef 1 2) (rot#get_coef 2 2);;

	let get_random_repere x y z = 
		(*renvoie un repere hortonormé aleatoire*)
		let repere = {e1 = new vector3D 1. 0. 0.;
			e2 = new vector3D 0. 1. 0.;
			e3 = new vector3D 0. 0. 1.;
			o = new vector3D x y z} and
			rotation = get_rotationMatrix (Random.float (2.*.pi)) (Random.float (2.*.pi)) (Random.float (2.*.pi)) in
			rotation_repere repere rotation;
			repere;;

	(*dans ce jeu, les vaisseaux ne pourrons s'orienter qu'en tournant autour de deux axes (tanguage et roulis)*)
	let ship_rotation pitch roll = 
		(*renvoie la matrice de rotation deduite des angles donnés*)
		let rotationX = create_matrix [|1.;0.;0.; 0.;cos roll;-.sin roll; 0.;sin roll;cos roll;|] (3,3) and
			rotationZ = create_matrix [|cos pitch;-.sin pitch;0.; sin pitch;cos pitch;0.; 0.;0.;1.;|] (3,3) in
		matrix_product rotationZ rotationX;;

	let get_camera_base repere =
		(*renvoie B(bc->b-cam) et le vecteur translation a apliquer pour deplacer les objet dans la b-cam*)
		let v = repere.o#copy3D () in
		v#dot (-.1.);
		let m = get_changement_de_base_matrix {e1 = repere.e1; e2 = repere.e2; e3 = repere.e3; o = v} in
		(*pour des repères orthonormé, la matrice inverse de la matrice de passage est aussi la transposéer
			On a donc rotation = transpose(B(b-cam->bc)) = B(bc->b-cam)*)
		{rotation = transpose_matrix m.rotation; translation = m.translation};;

"Camera et Render";;
	let create_camera size_x size_y ouverture = 
		(*créé une camera avec les reglages donné*)
		let a = (tf size_y) /. tf size_x and
			d = 1. /. tan(ouverture /. 2.) in
		let ad = a*.d in (*le coefficiant a permet de représenter la caméra par une base orthonormée dans bc et toutefois avoir un écran rectangulaire*)
		{size_x = size_x; size_y = size_y; ouverture = ouverture; a = a; d = d; ad = ad; cosO = cos (ouverture/.2.)};;

	let update_camera camera = 
		(*actualise la camera, permet de s'adapter a une fenetre de taille diferente, si le joueur est passer en plein écran par exemple*)
		let size_x = Graphics.size_x () and
			size_y = Graphics.size_y () in
		let a = (tf size_y) /. tf size_x in
		let ad = camera.d *. a in
		camera.size_x <- size_x;
		camera.size_y <- size_y;
		camera.a <- a;
		camera.ad <- ad;;

	let projection_on_screen vector camera = 
		(*projette un vecteur (donné dans b-cam) sur l'écran virtuelle*)
		let x = vector#get_x () and
			y = vector#get_y () and
			z = vector#get_z () in
		(*On applique Thalès, si le point est sur l'écran on obtient alors une valeur entre -1 et 1*)
		let proj_x = ti ((z*.camera.ad/.x +. 1.) *. tf camera.size_x /. 2.) and
			proj_y = ti ((y*.camera.d/.x +. 1.) *. tf camera.size_y /. 2.) in
			proj_x, proj_y;;

	let intersection_plan_droite normalPlan ptPlan directionDroite ptDroite = 
		(*le plan a pour equation: p.normalPlan - ptPlan.normalPlan = 0 et la droite: p = ptDroite + x*directionDroite*)
		let nv = scalar normalPlan directionDroite in
		if nv = 0. then failwith "le plan et la droite ne se coupe pas.";
		let v_ = directionDroite#copy3D () and
			x = ((-.scalar normalPlan ptPlan) -. scalar normalPlan ptDroite) /. nv in
		v_#dot x;
	 (vector_add ptDroite v_);;

	let intersection_plan_droite_optimiser d directionDroite ptDroite =
		(*pour le plan perpendiculaire à x (dans la base (x, y, z)) contenant le point (d,0,0)*)
		let nv = directionDroite#get_x () in
		if nv = 0. then failwith "le plan et la droite ne se coupe pas.";
		let v_ = directionDroite#copy3D () and
			x = (d -. ptDroite#get_x ()) /. nv in
		v_#dot x;
	 (vector_add ptDroite v_);;

	let intersection_segments x1 y1 x2 y2 x3 y3 x4 y4 = (*[(x1,y1),(x2,y2)] et [(x3,y3),(x4,y4)]*)
		(*en résolvant le systeme x1+t(x2-x1)=x3+s(x4-x3) et y1+t(y2-y1)=y3+s(y4-y3) (les équations des segments) on obtient les résultats suivant :*)
		let h = (x2-.x1)*.(y4-.y3)-.(x4-.x3)*.(y2-.y1) in
		if h = 0. then failwith "les segments sont paralelle";(*en réalité, si h est très faible les segments sont probablement paralèlles, mais cette ligne ne sert qu'à éviter une division par 0*)
		let s =(x1*.(y3-.y2)+.x2*.(y3-.y1)+.x3*.(y2-.y1))/.h and
			t =(x1*.(y3-.y4)+.x3*.(y4-.y1)+.x4*.(y1-.y3))/.h in
		s, t, x1+.t*.(x2-.x1), y1+.t*.(y2-.y1);;

	class icone nbPoints (camera : camera) radius= object (self)
		(*classe s'occupant de l'affichage des icone sur l'écran (un polygone à 'nbPoints' points*)
		val mutable angles = Array.make (nbPoints * 2 + 2) (0., 0.)
		val mutable points = Array.make (nbPoints * 2 + 2) (0, 0)
		val mutable radius = radius
		val mutable color = Graphics.white
		val mutable x = 0.
		val mutable y = 0.
		val mutable d = 0. (*permet de déterminer la taille de l'icône en fonction de la profondeur de l'objet*)
		val mutable inLife = true
		val camera = camera
		val nbPoints = nbPoints

		method die () = inLife <- false
		method is_in_life () = inLife
		method private clip_position x1 y1 x2 y2 = 
			(*si l'icone est hors de l'écran, on la place a la bordure de l'acran*)
			let s, t, x_, y_ = intersection_segments x1 y1 x2 y2 (tf camera.size_x /.2.) (tf camera.size_y /.2.) x y in
			if d<0. || (-.1.<s && s<1. && -.1.<t && t<1.) then begin
				x <- x_;
				y <- y_
			end
		method private clip_position_N r = 
			let yoffset = (tf camera.size_y -. r) in
			self#clip_position 0. yoffset (tf camera.size_x) yoffset
		method private clip_position_S r = 
			let yoffset = r in
			self#clip_position 0. yoffset (tf camera.size_x) yoffset
		method private clip_position_E r = 
			let xoffset = (tf camera.size_x -. r) in
			self#clip_position xoffset 0. xoffset (tf camera.size_y)
		method private clip_position_W r = 
			let xoffset = r in
			self#clip_position xoffset 0. xoffset (tf camera.size_y)
		method set_color color_ = color <- color_
		method set_position x_ y_ d_ = 
			x <- x_;
			y <- y_;
			d <- d_
		method private load_points () = 
			(*on calcule la postion des angles sur l'écran avant des les afficher*)
			let r = (tf (min camera.size_y camera.size_x) /. tf iconsPerScreen) *. radius /. (min (max iconeMin (abs_float d /. 2.)) iconeMax) in
			(*le rayon du polygone qui sera afficher a l'écran*)
			if camera.size_x <> 0 then begin
			(*on calcule le coté de l'écran pour lequel il faut verifier que l'icone ne sorte pas de l'écran*)
				let penteDiagonal = tf camera.size_y /. tf camera.size_x and
					x_ = x -. tf camera.size_x /. 2. and
					y_ = y -. tf camera.size_y /. 2. in
				if x_ <> 0. && abs_float (y_ /. x_) < penteDiagonal then 
					if x_ < 0. then self#clip_position_W r
					else self#clip_position_E r
				else if y_ < 0. then self#clip_position_S r
					else if y_ <> 0. then self#clip_position_N r
			end;
			(*finalement, on calcule la position des points sur l'écran*)
			for i = 0 to Array.length points - 1 do
				let ax, ay = angles.(i) in
				points.(i) <- (ti (x +. r *. ax)), (ti ( y +. r *. ay));
			done;
		method render () =
			(*affiche l'icone a l'écran*)
			self#load_points ();
			if nbPoints > 0 then begin
				Graphics.set_color color;
				Graphics.fill_poly points
			end

		initializer
			let a = 2. *. pi /. tf nbPoints in
			for i = 0 to nbPoints do
				angles.(i) <- (cos (a *. tf i), sin (a *. tf i))
			done;
			for i = (-1) to nbPoints - 1 do
				angles.(2 * nbPoints - i) <- (iconeSize *. cos (a *. tf i), iconeSize *. sin (a *. tf i))
			done;
			self#load_points ()
	end;;

	class iconeBuffer = object (self)
	(*le gestionnaire d'icone, rassemble toutes les icone du jeu et s'occupe de les afficher*)
		val mutable buffer = []

		method render () =
		(*affiche les icones*)
			foreach_list buffer 
				(fun ico -> ico#render ())
		method update () = 
		(*verifie que l'objet associé a l'icone est toujours en vie, sinon suprime l'icone*)
			let inLives = ref [] in
			foreach_list buffer 
				(fun ico -> if ico#is_in_life () then inLives := ico::!inLives);
			buffer <- !inLives
		method add_icone (icone : icone) = buffer <- icone::buffer
	end;;

	class point x y z = object (self)
		(*représente les points (3D) qui serons intégré dans les mailage*)
		(*les coordoné original sont donné dans b-obj*)
		val originalX = x
		val originalY = y
		val originalZ = z
		val mutable proj_x = 0
		val mutable proj_y = 0
		val mutable position = new vector3D x y z
		val mutable isProjected = false

		method move (movesMatrixs : movesMatrixs) = 
			(*applique le changement de base de B(b-obj->?) (avec ? = b-cam dans l'utilisation future) ainsi que la translation associé*)
			let v = matrix_product movesMatrixs.rotation position in
			position <- convert_vector3D (matrix_add v movesMatrixs.translation)
		method translate (vector : vector3D) = position <- (vector_add position vector)
		method get_projection camera = 
			(*renvoie la position du point sur l'écran, a l'aide de calcule légerement optimiser*)
			if not isProjected then begin 
				let px, py = projection_on_screen position camera in
				proj_x <- px;
				proj_y <- py;
				isProjected <- true
			end;
			proj_x, proj_y
		method get_vector () = position
		method get_deep () = (*renvoi la profondeur, (censé etre dans la b-cam) du point*)
			position#get_x ()
		method reset () = 
			(*Avant de calculer les projection et les changement de base, on place le point dans b-obj*)
			isProjected <- false;
			position <- new vector3D originalX originalY originalZ
	end;;

	class virtual material lightPower color = object (self)
		(*Cette classe s'occupe de la gestion des couleur d'une face*)
		val mutable lightPower = lightPower
		val mutable tempColor = {r = 0.; g = 0.; b = 0.}
		val color = color

		method reset () = (*renicialise la couleur entre deux affichage*)
			tempColor.r <- color.r;
			tempColor.g <- color.g;
			tempColor.b <- color.b;
		method add_color s c = (*permet d'ajouter un couleur, dans une certaine proportion*)
			let r1 = tempColor.r and 
				g1 = tempColor.g and 
				b1 = tempColor.b and
				r2 = c.r and 
				g2 = c.g and 
				b2 = c.b in
			tempColor.r <- r1*.(1.-.s) +. r2*.s;
			tempColor.g <- g1*.(1.-.s) +. g2*.s;
			tempColor.b <- b1*.(1.-.s) +. b2*.s;
		method aply_light coef light_color = 
			(*permet d'apliquer la lumière, modifie la couleur en fonction de l'impact qu'a la lumière sur ce materieaux*)
			self#add_color (coef*.lightPower) light_color
		method get_color () = 
			(*renvoie la couleur actuel*)
			let r = ti (tempColor.r**0.5) and
				g = ti (tempColor.g**0.5) and
				b = ti (tempColor.b**0.5) in
			Graphics.rgb r g b
		method set_color () = 
			(*initialise la couleur de Graphics avec la couuleur actuel*)
			Graphics.set_color (self#get_color ())
		method render x1 y1 x2 y2 x3 y3 =
			(*affiche un triangle avec la couleur actuelle*)
			self#set_color ();
			Graphics.fill_poly [|(x1,y1); (x2,y2); (x3,y3)|]
		initializer self#reset ();	
	end;;

	(*les differents materieaux utilisé pour le jeu*)
	class default = object (self) inherit material 0.5 (create_color 10 10 10) end;;
	class sunSurface = object (self) 
		inherit material 0. (create_color 200 40 20) 
			method render x1 y1 x2 y2 x3 y3 =
			Graphics.set_color (Graphics.rgb 200 40 20);
			Graphics.fill_poly [|(x1,y1); (x2,y2); (x3,y3)|];
			Graphics.set_color (Graphics.rgb 250 60 40);
			Graphics.draw_poly_line [|(x1,y1); (x2,y2); (x3,y3); (x1,y1)|]
	end;;
	class laserMaterial = object (self) inherit material 0.1 (create_color 100 200 100) end;;
	class explosionMaterial = object (self) inherit material 0.1 (create_color (240+Random.int 10) (190+Random.int 10) (140+Random.int 10)) end;;
	class bombeMaterial = object (self) inherit material 0.1 (create_color (50+Random.int 10) (50+Random.int 10) (100+Random.int 10)) end;;
	class bMainMaterial = object (self) inherit material 0.2 (create_color 100 100 100) end;;
	class bShieldMaterial = object (self) inherit material 0.1 (create_color 60 60 60) end;;
	class bLinkMaterial = object (self) inherit material 0.1 (create_color 100 75 75) end;;
	class bCokpitMaterial = object (self) inherit material 0.1 (create_color 100 50 50) end;;
	class kinOcean = object (self) inherit material 0.2 (create_color 5 20 50) end;;
	class kinContinent = object (self) inherit material 0.2 (create_color 50 50 5) end;;
	class kinPole = object (self) inherit material 0.2 (create_color 50 50 50) end;;
	class dCanon = object (self) inherit material 0.1 (create_color 60 20 20) end;;
	class dCanonSupport = object (self) inherit material 0.2 (create_color 40 40 40) end;;
	class dMain = object (self) inherit material 0.2 (create_color 50 50 80) end;;
	class dCokpit = object (self) inherit material 0.2 (create_color 50 50 50) end;;
	class hMain = object (self) inherit material 0.2 (create_color 100 100 100) end;;
	class hArm = object (self) inherit material 0.2 (create_color 150 100 50) end;;
	class hLink = object (self) inherit material 0.2 (create_color 80 80 90) end;;
	class hCokpit = object (self) inherit material 0.2 (create_color 100 50 50) end;; 

	let get_material materialType = match materialType with
	(*renvoi le materieux associé a la valeur de 'materialType'*)
		|Default -> new default
		|SunSurface -> new sunSurface
		|LaserMaterial -> new laserMaterial
		|ExplosionMaterial -> new explosionMaterial
		|BombeMaterial -> new bombeMaterial
		|BMainMaterial -> new bMainMaterial
		|BLinkMaterial -> new bLinkMaterial
		|BCokpitMaterial -> new bCokpitMaterial
		|BShieldMaterial -> new bShieldMaterial
		|KinOcean -> new kinOcean
		|KinContinent -> new kinContinent
		|KinPole -> new kinPole
		|DCanon -> new dCanon
		|DCanonSupport -> new dCanonSupport
		|DMain -> new dMain
		|DCokpit -> new dCokpit
		|HMain -> new hMain
		|HArm -> new hArm
		|HLink -> new hLink
		|HCokpit -> new hCokpit;;

	let switch a b = 
		(*echange 2 referance*)
		let temp = !a in
		a := !b;
		b := temp;; 

	class face3D p1 p2 p3 (material : material) camera environnementLocal = object (self)
	(*represente un enssemble de 3 point (3D), et permet de calculer efficassement la normal de cette face, ainsi que l'affichage de cette derniere*)
		val p1 : point = p1 
		val p2 : point = p2 
		val p3 : point = p3 
		val material = material
		val camera = camera
		val mutable sortingRank = max_float
		val mutable normal = get_nul_3D ()
		val mutable haveNormal = false
		val environnementLocal = environnementLocal

		method get_normal () =
		(*renvoie la normal de cette face de manière optimiser*)
			if not haveNormal then begin
				normal <- cross3D  (vector_sub (p3#get_vector ()) (p1#get_vector ()))  (vector_sub (p2#get_vector ()) (p1#get_vector ()));
				normal#normalize ();
				haveNormal <- true
			end; 
			normal
		method get_vect1 () = p1#get_vector ()
		method get_vect2 () = p2#get_vector ()
		method get_vect3 () = p3#get_vector ()
		method private projection_intersection_with_screen p1 p2 d camera = 
			(*Donne le projeté du point d'intersection entre le plan de projection et le segment [p1, p2]*)
			let p_ = (vector_sub (p1#get_vector ()) (p2#get_vector ())) in
			let p = intersection_plan_droite_optimiser d p_ (p2#get_vector ()) in
			projection_on_screen p camera
		method private face_appear_on_screen x1 y1 x2 y2 x3 y3 = 
			(*vrai si le triangle apparait effectivement sur l'écran (grossierement, il y a des faux positife, mais pas de faux negatif)*)
			not ((x1 < 0 && x2 < 0 && x3 < 0) ||
				(camera.size_x < x1 && camera.size_x < x2 && camera.size_x < x3) ||
				(y1 < 0 && y2 < 0 && y3 < 0) ||
				(camera.size_y < y1 && camera.size_y < y2 && camera.size_y < y3))
		method render () = 
			(*affiche (ou pas si le triangle n'est pas sur l'écran) la face a l'écran*)
			let cunt = ref 0 and
				d = 0.001 in
			if p1#get_deep () <= d then incr cunt;
			if p2#get_deep () <= d then incr cunt;
			if p3#get_deep () <= d then incr cunt;
			(*on conte les points qui sont derière la camera, car si la face 'traverse' la camera, il faut recouper cette derniere avant de l'afficher*)
			if !cunt <> 3 then begin (*s'il y a au moins un point de la face en face du joueur*)
				let p1_ = ref p1 and
					p2_ = ref p2 and
					p3_ = ref p3 in

				let s = 0.5+.(scalar environnementLocal.lightDirection (self#get_normal ()))/.2. in
				material#aply_light s environnementLocal.lightColor;

				if !cunt = 0 then begin(*les trois points sont face au joueur*)
					let x1, y1 = p1#get_projection camera and	
						x2, y2 = p2#get_projection camera and	
						x3, y3 = p3#get_projection camera in
					if self#face_appear_on_screen x1 y1 x2 y2 x3 y3 then material#render x1 y1 x2 y2 x3 y3;
				end;

				if !cunt = 1 then begin(*seulement 2 points faces au joueur*)
					if p2#get_deep () <= d then switch p1_ p2_;
					if p3#get_deep () <= d then switch p1_ p3_;
					(*alors p1_ est le seul point derrière l'observateur*)
					let x1, y1 = !p2_#get_projection camera and	
						x2, y2 = !p3_#get_projection camera and
						x3, y3 = self#projection_intersection_with_screen !p1_ !p2_ d camera and
						x4, y4 = self#projection_intersection_with_screen !p1_ !p3_ d camera in
					if self#face_appear_on_screen x1 y1 x2 y2 x3 y3 then material#render x1 y1 x2 y2 x3 y3;
					if self#face_appear_on_screen x3 y3 x4 y4 x2 y2 then material#render x3 y3 x4 y4 x2 y2;
				end;

				if !cunt = 2 then begin(*un seul point face au joueur*)
					if p2#get_deep () >= d then switch p1_ p2_;
					if p3#get_deep () >= d then switch p1_ p3_;
					(*alors p1_ est le seul point devant l'observateur*)
					let x1, y1 = !p1_#get_projection camera and	
						x2, y2 = self#projection_intersection_with_screen !p1_ !p2_ d camera and
						x3, y3 = self#projection_intersection_with_screen !p1_ !p3_ d camera in
					if self#face_appear_on_screen x1 y1 x2 y2 x3 y3 then material#render x1 y1 x2 y2 x3 y3;
				end
			end
		method is_face_player () = 
			(*vrai si cette face est orientée "face" au joueur*)
			scalar (self#get_normal ()) (p1#get_vector ()) > 0.
		method compute_sortingRank () = 
			(*calcule la valeur associé a cette face, permettant aprés d'afficher les face de la plus éloigné a la plus proche*)
			sortingRank <- max (p1#get_deep ()) (max (p2#get_deep ()) (p3#get_deep ()))
		method get_sortingRank () = sortingRank
		method reset () = 
			(*renitialise la face entre deux calcul de render*)
			haveNormal <- false;
			material#reset ()
	end;;

	let sorting_function f1 f2 = 
		(*fonction qui vas trié les face entre elles*)
		f1#get_sortingRank () >= f2#get_sortingRank ();;

	class faceBuffer = object (self)
		(*A chaque tour de boucle (d'affichage) cette fonction vas accumulé toutes les faces a afficher et les afficher dans le bonne ordre*)
		val mutable buffer = []

		method add_face (f : face3D) = buffer <- f::buffer
		method render () = 
			(*affiche les faces*)
			let buf = Array.of_list buffer in
			foreach_array buf (fun face -> face#compute_sortingRank ());
			quick_sort buf sorting_function 0 (Array.length buf);
			foreach_array buf (fun face -> face#render ())
		method reset () = 
			(*renitialise le buffer entre deux tour de boucle*)
			buffer <- []
	end;;

"Stars et mesh";;
	class star camera cameraRepere = object (self)
		(*représente une étoile dans le ciel*)
		val direction = new vector3D (Random.float 1. -. 0.5) (Random.float 1. -. 0.5) (Random.float 1. -. 0.5)
		val cameraRepere = cameraRepere
		val camera = camera

		method render (rotationMatrix : matrix) = 
			(*affiche l'étoile a l'écran en fonction de la matrice B(bc->b-cam)*)
			if self#is_face_player () then begin
				let r = (min camera.size_y camera.size_x)/starPerScreen and
					p = convert_vector3D (matrix_product rotationMatrix direction) in
				let proj_x, proj_y = projection_on_screen p camera in
				Graphics.fill_circle proj_x proj_y r
			end
		method is_face_player () = 
			(*permet de savoir si cette étoile est face au joueur, ou pas fin de poursuivre les calcul de projection, ou pas*)
			scalar cameraRepere.e1 direction > 0.
		initializer direction#normalize ()
	end;;

	class starBuffer nbStars camera cameraRepere = object (self)
		(*gestionnaire d'étoiles, cette classe vas s'occuper d'afficher les étoile a afficher*)
		val mutable buffer = Array.make nbStars (new star camera cameraRepere)
		val camera = camera
		val cameraRepere = cameraRepere

		method render (rotationMatrix : matrix) = 
			foreach_array buffer (fun star -> star#render rotationMatrix)
		initializer
			for i = 0 to nbStars - 1 do
				buffer.(i) <- (new star camera cameraRepere)
			done
	end;;

	class mesh points faces faceBuffer environnementLocal = object (self)
		(*représente un objet 3D, (une planette, un vaisseau...)*)
		val points : point array = points (*les points constituant l'objet*)
		val faces : face3D array = faces (*les faces reliant les points entre eux*)
		val faceBuffer : faceBuffer = faceBuffer (*le gestionnaire de face dans lequel il faut placer les face pour qu'elle soit afficher*)
		val environnementLocal : environnementLocal = environnementLocal (*la direction local de la lumière. le calcul de la lumière est fait par objet 3D et pas par face, permettant une économie notable de calcul pour la machine*)

		method get_environnementLocal () = environnementLocal
		method move (movesMatrixs : movesMatrixs) = 
			(*place les points dans la base dont la matrice de passage est donné*)
			foreach_array points (fun point -> point#move movesMatrixs)
		method move_opty (moveInCartesian : movesMatrixs) (movesInCamera : movesMatrixs) = 
			(*permet d'effectuer les changement de base: B(b-obj->bc) et B(bc->b-cam) (ainsi que les translation associé) avec un seul calcul de changement de base âr points (au lieu de deux)*)
			let rot = matrix_product movesInCamera.rotation moveInCartesian.rotation and
				trans = convert_vector3D (matrix_product movesInCamera.rotation (vector_add moveInCartesian.translation movesInCamera.translation)) in
				(*la translation est composé de la translation pour passer de b-obj->bc ainsi que la translation pour passer de bc->b-cam (vecteurs qui seront finalement écrit dans la b-cam)*)
			let mvsMat = {rotation = rot; translation = trans} in
			foreach_array points (fun point -> point#move mvsMat)
		method  translate (vector : vector3D) = 
			foreach_array points (fun point -> point#translate vector)
		method reset () = 
			(*renitialise le maillage entre deux calcul de rendu*)
			foreach_array points (fun point -> point#reset ());
			foreach_array faces (fun face -> face#reset ())
		method put_faces_in_buffer () = 
			(*place les face dans la faceBuffer si neccessaire*)
			foreach_array faces (fun face -> if face#is_face_player () then faceBuffer#add_face face)
		method get_faceBuffer () = faceBuffer 
		method get_points () = points
		method get_faces () = faces
	end;;

	type lineType = None | Point | Face | EditMaterial;;
	let stf = float_of_string and
		sti = int_of_string;;
	let create_mesh_from_obj obj faceBuffer camera matrix environnementLocal = 
		(*algorithme permettant de lire les .obj et d'appliquer les materieux adapter*)
		let sep = Str.regexp "\n" and
			space = Str.regexp " " in
		let lines = ref (Str.split sep obj) in
		let pointsList = ref [] and
			facesList = ref [] and
			points = ref [||] and
			faces = ref [||] and
			lastLines = ref None and
			currentLine = ref None and
			currentMaterial = ref Default in
		let updateMaterial l =
			let [_; mat] = Str.split space l in
			match mat with
	 			|"sunSurface" -> currentMaterial := SunSurface
	 			|"laserMaterial"  -> currentMaterial := LaserMaterial
	 			|"explosionMaterial" -> currentMaterial := ExplosionMaterial
	 			|"bombe" -> currentMaterial := BombeMaterial
	 			|"bCokpit" -> currentMaterial := BCokpitMaterial
	 			|"bLink" -> currentMaterial := BLinkMaterial
	 			|"bMain" -> currentMaterial := BMainMaterial
	 			|"bShield" -> currentMaterial := BShieldMaterial
	 			|"kinOcean" -> currentMaterial := KinOcean
	 			|"kinContinent" -> currentMaterial := KinContinent
	 			|"kinPole" -> currentMaterial := KinPole
	 			|"dCanon" -> currentMaterial := DCanon
				|"dCanonSupport" -> currentMaterial := DCanonSupport
				|"dMain" -> currentMaterial := DMain
				|"dCokpit" -> currentMaterial := DCokpit
				|"hMain" -> currentMaterial := HMain
				|"hArm" -> currentMaterial := HArm
				|"hLink" -> currentMaterial := HLink
				|"hCokpit" -> currentMaterial := HCokpit
				|_ -> currentMaterial := Default
			in
		while !lines <> [] do
			let l::t = !lines in
			lines := t;
			if String.length l >= 2 then begin
				begin
				currentLine := match l.[0], l.[1] with
					|'v',' ' -> Point
					|'f',' ' -> Face
					|'u','s' -> EditMaterial
					| _ , _  -> None
				end;
				if !currentLine <> !lastLines then begin
					match !lastLines with
						|Point  -> points  := Array.of_list (List.rev !pointsList)
						|Face   -> faces   := Array.of_list !facesList
						|EditMaterial -> ()
						|None   -> ()
				end;
				lastLines := !currentLine;
				begin match !lastLines with
					|Point  -> 
						let [_;x;y;z] = Str.split space l in 
						let p_ = matrix_add  matrix.translation (create_matrix [|(stf x);(stf y);(stf z)|] (3,1)) in
						let p = matrix_product matrix.rotation p_ in
						pointsList := (new point (p#get_coef 0 0) (p#get_coef 1 0) (p#get_coef 2 0))::!pointsList
					|Face   -> 
						let [_;p1;p2;p3] = Str.split space l in 
						facesList := (new face3D !points.(sti p1 - 1) !points.(sti p2 - 1) !points.(sti p3 - 1) (get_material !currentMaterial) camera environnementLocal)::!facesList
					|EditMaterial -> updateMaterial l
					|None   -> ();
				end;
			end
		done;
		new mesh !points (Array.of_list !facesList) faceBuffer environnementLocal;;

	let read_file file = 
		(*algorithme permettant de charger les .obj*)
		let txt = ref "" in
		let chanel = open_in file in
		try
			while true do
	    		txt := !txt^"\n"^input_line chanel
	  		done;
	  		!txt
		with End_of_file ->
			close_in chanel;
			!txt ;;

	(*chargement des .obj situé sur la machine*)
	(* let bombarder_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/bombarder.obj";;
	let hunter_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/hunter.obj";;
	let destroyer_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/destroyer.obj";;
	let laser_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/laser.obj";;
	let bombe_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/bombe.obj";;
	let shpere_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/shpere.obj";;
	let alpha_42_phb_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/alpha_42_phb.obj";;
	let kin_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/kin.obj";;
	let explosion_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/explosion.obj";;
	let cokpit_0_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/cokpit_0.obj";;
	let cokpit_1_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/cokpit_1.obj";;
	let cokpit_2_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/cokpit_2.obj";;
	let cokpit_3_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/cokpit_3.obj";;
	let cokpit_4_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/cokpit_4.obj";;
	let cokpit_5_obj = read_file "C:/Users/pierr/OCaml workspace/Évwâyù/cokpit_5.obj";; *)

	(*les .obj sont codé en dur pour ne fournir qu'un seul fichier*)
 	let bombarder_obj = "# Blender v2.79 (sub 0) OBJ File: 'bombarder8.blend'\n# www.blender.org\nmtllib bombarder8.mtl\nv 0.138309 0.000000 0.133634\nv 0.138309 -0.000000 0.077677\nv 0.162539 0.000000 0.091666\nv 0.162539 0.000000 0.119645\nv 0.138309 0.132899 0.000736\nv 0.138309 0.076941 0.000736\nv 0.162539 0.090931 0.000736\nv 0.162539 0.118909 0.000736\nv 0.138309 0.000000 -0.132163\nv 0.138309 0.000000 -0.076206\nv 0.162539 0.000000 -0.090195\nv 0.162539 0.000000 -0.118174\nv 0.138309 -0.132899 0.000736\nv 0.138309 -0.076941 0.000736\nv 0.162539 -0.090931 0.000736\nv 0.162539 -0.118909 0.000736\nv 0.138309 0.000000 0.140629\nv 0.138309 0.000000 0.350469\nv 0.068363 -0.121151 0.140629\nv 0.068362 -0.121151 0.350469\nv -0.071531 -0.121151 0.140629\nv -0.071531 -0.121151 0.350469\nv -0.141477 0.000000 0.140629\nv -0.141477 -0.000000 0.350469\nv -0.071531 0.121151 0.140629\nv -0.071531 0.121151 0.350469\nv 0.068362 0.121151 0.140629\nv 0.068362 0.121151 0.350469\nv 0.138309 -0.000000 -0.348997\nv 0.138309 -0.000000 -0.139158\nv 0.068363 -0.121151 -0.348997\nv 0.068363 -0.121151 -0.139158\nv -0.071531 -0.121151 -0.348997\nv -0.071531 -0.121151 -0.139158\nv -0.141477 -0.000000 -0.348997\nv -0.141477 -0.000000 -0.139158\nv -0.071531 0.121151 -0.348997\nv -0.071531 0.121151 -0.139158\nv 0.068363 0.121151 -0.348997\nv 0.068362 0.121151 -0.139158\nv 0.418096 -0.000000 -0.348997\nv 0.208256 0.363453 -0.348998\nv -0.211424 0.363453 -0.348998\nv -0.421264 -0.000000 -0.348998\nv -0.001584 -0.000000 -0.488891\nv -0.211424 -0.363453 -0.348998\nv 0.208256 -0.363453 -0.348998\nv 0.418096 0.000000 0.350469\nv 0.208256 -0.363453 0.350469\nv -0.211424 -0.363453 0.350469\nv -0.421264 0.000000 0.350469\nv -0.001584 -0.000000 0.490362\nv -0.211424 0.363453 0.350469\nv 0.208256 0.363453 0.350469\nv 0.138309 -0.139893 0.140629\nv -0.141477 -0.139893 0.140629\nv -0.141477 -0.139893 -0.139158\nv 0.138309 -0.139893 -0.139158\nv 0.138309 0.139893 0.140629\nv -0.141477 0.139893 0.140629\nv -0.141477 0.139893 -0.139158\nv 0.138309 0.139893 -0.139158\nusemtl bCokpit\nf 6 3 2\nf 7 4 3\nf 8 1 4\nf 10 7 6\nf 11 8 7\nf 12 5 8\nf 14 11 10\nf 15 12 11\nf 16 9 12\nf 2 15 14\nf 3 16 15\nf 16 1 13\nf 6 7 3\nf 7 8 4\nf 8 5 1\nf 10 11 7\nf 11 12 8\nf 12 9 5\nf 14 15 11\nf 15 16 12\nf 16 13 9\nf 2 3 15\nf 3 4 16\nf 16 4 1\nusemtl bLink\nf 18 19 17\nf 20 21 19\nf 22 23 21\nf 24 25 23\nf 26 27 25\nf 28 17 27\nf 30 31 29\nf 32 33 31\nf 34 35 33\nf 36 37 35\nf 38 39 37\nf 40 29 39\nf 18 20 19\nf 20 22 21\nf 22 24 23\nf 24 26 25\nf 26 28 27\nf 28 18 17\nf 30 32 31\nf 32 34 33\nf 34 36 35\nf 36 38 37\nf 38 40 39\nf 40 30 29\nusemtl bShield\nf 41 45 42\nf 42 45 43\nf 43 45 44\nf 44 45 46\nf 46 45 47\nf 47 45 41\nf 42 44 47\nf 47 41 42\nf 42 43 44\nf 44 46 47\nusemtl bShield\nf 48 52 49\nf 49 52 50\nf 50 52 51\nf 51 52 53\nf 53 52 54\nf 54 52 48\nf 49 51 54\nf 54 48 49\nf 49 50 51\nf 51 53 54\nusemtl bMain\nf 55 57 58\nf 62 60 59\nf 59 56 55\nf 60 57 56\nf 57 62 58\nf 55 62 59\nf 55 56 57\nf 62 61 60\nf 59 60 56\nf 60 61 57\nf 57 61 62\nf 55 58 62\n";;
	let hunter_obj = "# Blender v2.79 (sub 0) OBJ File: 'hunter4.blend'\n# www.blender.org\nmtllib hunter4.mtl\nv 0.102288 -0.097608 0.000000\nv 0.120084 -0.087334 0.000000\nv 0.120084 -0.066784 0.000000\nv 0.102288 -0.056510 0.000000\nv 0.102288 -0.030163 -0.092831\nv 0.120084 -0.026988 -0.083059\nv 0.120084 -0.020638 -0.063516\nv 0.102288 -0.017463 -0.053744\nv 0.102288 0.078967 -0.057373\nv 0.120084 0.070654 -0.051333\nv 0.120084 0.054030 -0.039255\nv 0.102288 0.045718 -0.033216\nv 0.102288 0.078967 0.057373\nv 0.120084 0.070654 0.051333\nv 0.120084 0.054030 0.039255\nv 0.102288 0.045718 0.033216\nv 0.102288 -0.030163 0.092831\nv 0.120084 -0.026988 0.083059\nv 0.120084 -0.020638 0.063516\nv 0.102288 -0.017463 0.053744\nv -0.103203 -0.038530 0.344197\nv -0.103203 0.038530 0.344197\nv -0.103203 -0.038530 0.102745\nv -0.103203 0.038530 0.102745\nv 0.102288 -0.038530 0.344197\nv 0.102288 0.038530 0.344197\nv 0.102288 -0.038530 0.102745\nv 0.102288 0.038530 0.102745\nv -0.103203 -0.038530 -0.102745\nv -0.103203 0.038530 -0.102745\nv -0.103203 -0.038530 -0.344197\nv -0.103203 0.038530 -0.344197\nv 0.102288 -0.038530 -0.102745\nv 0.102288 0.038530 -0.102745\nv 0.102288 -0.038530 -0.344197\nv 0.102288 0.038530 -0.344197\nv 0.102288 -0.102745 -0.102745\nv 0.102288 -0.102745 0.102745\nv -0.103203 -0.102745 0.102745\nv -0.103203 -0.102745 -0.102745\nv 0.102288 0.102745 -0.102745\nv 0.102288 0.102745 0.102745\nv -0.103203 0.102745 0.102745\nv -0.103203 0.102745 -0.102745\nv 0.205033 0.077059 -0.410981\nv 0.205033 0.038530 -0.477716\nv 0.205033 -0.038530 -0.477716\nv 0.205033 -0.077059 -0.410981\nv 0.410524 -0.000000 -0.410981\nv 0.205033 -0.038529 -0.344246\nv 0.205033 0.038530 -0.344246\nv -0.205948 0.077059 -0.410981\nv 0.205033 0.077059 -0.410981\nv -0.205948 0.038530 -0.477716\nv 0.205033 0.038529 -0.477717\nv -0.205948 -0.038530 -0.477716\nv 0.205033 -0.038530 -0.477716\nv -0.205948 -0.077059 -0.410981\nv 0.205033 -0.077059 -0.410981\nv -0.205948 -0.038529 -0.344246\nv 0.205033 -0.038530 -0.344246\nv -0.205948 0.038530 -0.344246\nv 0.205033 0.038529 -0.344246\nv 0.205033 0.077059 0.410981\nv 0.205033 0.038530 0.344246\nv 0.205033 -0.038530 0.344246\nv 0.205033 -0.077059 0.410981\nv 0.410524 -0.000000 0.410981\nv 0.205033 -0.038529 0.477716\nv 0.205033 0.038530 0.477716\nv -0.205948 0.077059 0.410981\nv 0.205033 0.077059 0.410981\nv -0.205948 0.038530 0.344246\nv 0.205033 0.038529 0.344246\nv -0.205948 -0.038530 0.344246\nv 0.205033 -0.038530 0.344246\nv -0.205948 -0.077059 0.410981\nv 0.205033 -0.077059 0.410981\nv -0.205948 -0.038529 0.477717\nv 0.205033 -0.038530 0.477716\nv -0.205948 0.038530 0.477717\nv 0.205033 0.038529 0.477716\nusemtl hCokpit\ns off\nf 1 6 2\nf 6 3 2\nf 3 8 4\nf 9 6 5\nf 10 7 6\nf 11 8 7\nf 13 10 9\nf 14 11 10\nf 15 12 11\nf 17 14 13\nf 18 15 14\nf 15 20 16\nf 1 18 17\nf 2 19 18\nf 3 20 19\nf 1 5 6\nf 6 7 3\nf 3 7 8\nf 9 10 6\nf 10 11 7\nf 11 12 8\nf 13 14 10\nf 14 15 11\nf 15 16 12\nf 17 18 14\nf 18 19 15\nf 15 19 20\nf 1 2 18\nf 2 3 19\nf 3 4 20\nusemtl hLink\nf 22 23 21\nf 28 25 27\nf 27 21 23\nf 24 26 28\nf 22 24 23\nf 28 26 25\nf 27 25 21\nf 24 22 26\nusemtl hLink\nf 30 31 29\nf 36 33 35\nf 35 29 31\nf 32 34 36\nf 30 32 31\nf 36 34 33\nf 35 33 29\nf 32 30 34\nusemtl hMain\nf 37 39 40\nf 44 42 41\nf 41 38 37\nf 42 39 38\nf 39 44 40\nf 37 44 41\nf 37 38 39\nf 44 43 42\nf 41 42 38\nf 42 43 39\nf 39 43 44\nf 37 40 44\nusemtl hArm\nf 45 49 46\nf 46 49 47\nf 47 49 48\nf 48 49 50\nf 50 49 51\nf 51 49 45\nf 53 54 52\nf 55 56 54\nf 57 58 56\nf 59 60 58\nf 61 62 60\nf 63 52 62\nf 54 58 62\nf 53 55 54\nf 55 57 56\nf 57 59 58\nf 59 61 60\nf 61 63 62\nf 63 53 52\nf 62 52 54\nf 54 56 58\nf 58 60 62\nusemtl hArm\nf 64 68 65\nf 65 68 66\nf 66 68 67\nf 67 68 69\nf 69 68 70\nf 70 68 64\nf 72 73 71\nf 74 75 73\nf 76 77 75\nf 78 79 77\nf 80 81 79\nf 82 71 81\nf 73 77 81\nf 72 74 73\nf 74 76 75\nf 76 78 77\nf 78 80 79\nf 80 82 81\nf 82 72 71\nf 81 71 73\nf 73 75 77\nf 77 79 81\n";; 
	let destroyer_obj = "# Blender v2.79 (sub 0) OBJ File: 'destroyer8.blend.blend'\n# www.blender.org\nmtllib destroyer8.blend.mtl\nv 0.092903 0.043502 0.014587\nv 0.200321 0.089098 0.014587\nv 0.092903 0.043502 -0.014587\nv 0.200321 0.089098 -0.014587\nv 0.105668 0.017411 0.014587\nv 0.213086 0.063007 0.014587\nv 0.105668 0.017411 -0.014587\nv 0.213086 0.063007 -0.014587\nv 0.046880 0.070939 -0.131281\nv 0.046880 0.070939 0.131281\nv 0.088138 0.053849 -0.131281\nv 0.088138 0.053849 0.131281\nv 0.105228 0.012591 -0.131281\nv 0.105228 0.012591 0.131281\nv -0.011467 0.012591 -0.131281\nv -0.011467 0.012591 0.131281\nv 0.005622 0.053849 -0.131281\nv 0.005622 0.053849 0.131281\nv -0.171922 0.158460 -0.145868\nv -0.045597 0.158460 -0.072934\nv -0.045597 0.158460 0.072934\nv -0.171922 0.158460 0.145868\nv -0.171922 0.216807 0.000000\nv -0.298248 0.158460 0.072934\nv -0.298248 0.158460 -0.072934\nv -0.171922 0.012591 -0.145868\nv -0.171922 0.158460 -0.145868\nv -0.045597 0.012591 -0.072934\nv -0.045597 0.158460 -0.072934\nv -0.045597 0.012591 0.072934\nv -0.045597 0.158460 0.072934\nv -0.171922 0.012591 0.145868\nv -0.171922 0.158460 0.145868\nv -0.298248 0.012591 0.072934\nv -0.298248 0.158460 0.072934\nv -0.298248 0.012591 -0.072934\nv -0.298248 0.158460 -0.072934\nv -0.390725 -0.060343 0.291737\nv -0.390725 0.012591 0.291737\nv -0.390725 -0.060343 -0.291737\nv -0.390725 0.012591 -0.291737\nv 0.484485 -0.206211 0.072934\nv 0.484485 0.012591 0.072934\nv 0.484485 -0.206211 -0.072934\nv 0.484485 0.012591 -0.072934\nv 0.092903 0.043502 0.102108\nv 0.200321 0.089098 0.102108\nv 0.092903 0.043502 0.072934\nv 0.200321 0.089098 0.072934\nv 0.105668 0.017411 0.102108\nv 0.213086 0.063007 0.102108\nv 0.105668 0.017411 0.072934\nv 0.213086 0.063007 0.072934\nv 0.092903 0.043502 -0.072934\nv 0.200321 0.089098 -0.072934\nv 0.092903 0.043502 -0.102108\nv 0.200321 0.089098 -0.102108\nv 0.105668 0.017411 -0.072934\nv 0.213086 0.063007 -0.072934\nv 0.105668 0.017411 -0.102108\nv 0.213086 0.063007 -0.102108\nv -0.095456 -0.199758 -0.074279\nv -0.095456 -0.199758 -0.100763\nv -0.117482 -0.214256 -0.074279\nv -0.117482 -0.214256 -0.100763\nv -0.167467 -0.181986 -0.087521\nv -0.120288 -0.157554 -0.056847\nv -0.177748 -0.147422 -0.037888\nv -0.213261 -0.141160 -0.087521\nv -0.177748 -0.147422 -0.137154\nv -0.120288 -0.157554 -0.118195\nv -0.136923 -0.101628 -0.037888\nv -0.194383 -0.091496 -0.056847\nv -0.194383 -0.091496 -0.118195\nv -0.136923 -0.101628 -0.137154\nv -0.101410 -0.107890 -0.087521\nv -0.144870 -0.170284 -0.074279\nv -0.144870 -0.170284 -0.100763\nv -0.121388 -0.158123 -0.100763\nv -0.121388 -0.158123 -0.074279\nv -0.095456 -0.199758 0.100763\nv -0.095456 -0.199758 0.074279\nv -0.117482 -0.214256 0.100763\nv -0.117482 -0.214256 0.074279\nv -0.167467 -0.181986 0.087521\nv -0.120288 -0.157554 0.118195\nv -0.177748 -0.147422 0.137154\nv -0.213261 -0.141160 0.087521\nv -0.177748 -0.147422 0.037888\nv -0.120288 -0.157554 0.056847\nv -0.136923 -0.101628 0.137154\nv -0.194383 -0.091496 0.118195\nv -0.194383 -0.091496 0.056847\nv -0.136923 -0.101628 0.037888\nv -0.101410 -0.107890 0.087521\nv -0.144870 -0.170284 0.100763\nv -0.144870 -0.170284 0.074279\nv -0.121388 -0.158123 0.074279\nv -0.121388 -0.158123 0.100763\nusemtl dCanon\nf 2 3 1\nf 4 7 3\nf 8 5 7\nf 5 2 1\nf 4 6 8\nf 2 4 3\nf 4 8 7\nf 8 6 5\nf 5 6 2\nf 4 2 6\nusemtl dCanonSupport\nf 10 11 9\nf 12 13 11\nf 10 16 14\nf 11 15 17\nf 16 17 15\nf 18 9 17\nf 10 12 11\nf 12 14 13\nf 14 12 10\nf 10 18 16\nf 17 9 11\nf 11 13 15\nf 16 18 17\nf 18 10 9\nusemtl dCokpit\nf 19 23 20\nf 20 23 21\nf 21 23 22\nf 22 23 24\nf 24 23 25\nf 25 23 19\nusemtl dCokpit\nf 27 28 26\nf 29 30 28\nf 31 32 30\nf 33 34 32\nf 35 36 34\nf 37 26 36\nf 27 29 28\nf 29 31 30\nf 31 33 32\nf 33 35 34\nf 35 37 36\nf 37 27 26\nusemtl dMain\nf 39 40 38\nf 40 45 44\nf 45 42 44\nf 43 38 42\nf 44 38 40\nf 41 43 45\nf 39 41 40\nf 40 41 45\nf 45 43 42\nf 43 39 38\nf 44 42 38\nf 41 39 43\nusemtl dCanon\nf 47 48 46\nf 49 52 48\nf 53 50 52\nf 50 47 46\nf 49 51 53\nf 47 49 48\nf 49 53 52\nf 53 51 50\nf 50 51 47\nf 49 47 51\nusemtl dCanon\nf 55 56 54\nf 57 60 56\nf 61 58 60\nf 58 55 54\nf 57 59 61\nf 55 57 56\nf 57 61 60\nf 61 59 58\nf 58 59 55\nf 57 55 59\nusemtl dCanonSupport\nf 63 64 65\nf 66 67 68\nf 66 68 69\nf 66 69 70\nf 66 70 71\nf 67 71 76\nf 68 67 72\nf 69 68 73\nf 70 69 74\nf 71 70 75\nf 67 76 72\nf 68 72 73\nf 69 73 74\nf 70 74 75\nf 71 75 76\nusemtl dCanon\nf 78 64 77\nf 79 65 78\nf 80 63 79\nf 80 64 62\nf 67 66 71\nf 63 62 64\nf 78 65 64\nf 79 63 65\nf 80 62 63\nf 80 77 64\nusemtl dCanonSupport\nf 82 83 84\nf 85 86 87\nf 85 87 88\nf 85 88 89\nf 85 89 90\nf 86 90 95\nf 87 86 91\nf 88 87 92\nf 89 88 93\nf 90 89 94\nf 86 95 91\nf 87 91 92\nf 88 92 93\nf 89 93 94\nf 90 94 95\nusemtl dCanon\nf 97 83 96\nf 98 84 97\nf 99 82 98\nf 99 83 81\nf 86 85 90\nf 82 81 83\nf 97 84 83\nf 98 82 84\nf 99 81 82\nf 99 96 83\n";; 
	let laser_obj = "# Blender v2.79 (sub 0) OBJ File: ''\n# www.blender.org\nv -0.500000 -0.500000 0.500000\nv -0.500000 0.500000 0.500000\nv -0.500000 -0.500000 -0.500000\nv -0.500000 0.500000 -0.500000\nv 0.500000 -0.500000 0.500000\nv 0.500000 0.500000 0.500000\nv 0.500000 -0.500000 -0.500000\nv 0.500000 0.500000 -0.500000\nusemtl laserMaterial\ns off\nf 2 3 1\nf 4 7 3\nf 8 5 7\nf 6 1 5\nf 7 1 3\nf 4 6 8\nf 2 4 3\nf 4 8 7\nf 8 6 5\nf 6 2 1\nf 7 5 1\nf 4 2 6\n";; 
	let bombe_obj = "# Blender v2.79 (sub 0) OBJ File: ''\n# www.blender.org\nv 0.000000 -0.500000 0.000000\nv 0.361800 -0.223607 0.262860\nv -0.138193 -0.223607 0.425320\nv -0.447212 -0.223607 0.000000\nv -0.138193 -0.223607 -0.425320\nv 0.361800 -0.223607 -0.262860\nv 0.138193 0.223607 0.425320\nv -0.361800 0.223607 0.262860\nv -0.361800 0.223607 -0.262860\nv 0.138193 0.223607 -0.425320\nv 0.447212 0.223607 0.000000\nv 0.000000 0.500000 0.000000\nusemtl bombe\ns off\nf 1 2 3\nf 2 1 6\nf 1 3 4\nf 1 4 5\nf 1 5 6\nf 2 6 11\nf 3 2 7\nf 4 3 8\nf 5 4 9\nf 6 5 10\nf 2 11 7\nf 3 7 8\nf 4 8 9\nf 5 9 10\nf 6 10 11\nf 7 11 12\nf 8 7 12\nf 9 8 12\nf 10 9 12\nf 11 10 12\n";; 
	let alpha_42_phb_obj = "# Blender v2.79 (sub 0) OBJ File: ''\n# www.blender.org\nv 0.000000 -0.500000 0.000000\nv 0.361800 -0.223607 0.262860\nv -0.138193 -0.223607 0.425320\nv -0.447212 -0.223607 0.000000\nv -0.138193 -0.223607 -0.425320\nv 0.361800 -0.223607 -0.262860\nv 0.138193 0.223607 0.425320\nv -0.361800 0.223607 0.262860\nv -0.361800 0.223607 -0.262860\nv 0.138193 0.223607 -0.425320\nv 0.447212 0.223607 0.000000\nv 0.000000 0.500000 0.000000\nusemtl sunSurface\ns off\nf 1 2 3\nf 2 1 6\nf 1 3 4\nf 1 4 5\nf 1 5 6\nf 2 6 11\nf 3 2 7\nf 4 3 8\nf 5 4 9\nf 6 5 10\nf 2 11 7\nf 3 7 8\nf 4 8 9\nf 5 9 10\nf 6 10 11\nf 7 11 12\nf 8 7 12\nf 9 8 12\nf 10 9 12\nf 11 10 12\n";; 
	let kin_obj = "# Blender v2.79 (sub 0) OBJ File: ''\n# www.blender.org\nv 0.000000 -0.500000 0.000000\nv 0.361804 -0.223610 0.262863\nv -0.138194 -0.223610 0.425325\nv -0.447213 -0.223608 0.000000\nv -0.138194 -0.223610 -0.425325\nv 0.361804 -0.223610 -0.262863\nv 0.138194 0.223610 0.425325\nv -0.361804 0.223610 0.262863\nv -0.361804 0.223610 -0.262863\nv 0.138194 0.223610 -0.425325\nv 0.447213 0.223608 0.000000\nv 0.000000 0.500000 0.000000\nv -0.081228 -0.425327 0.249998\nv 0.212661 -0.425327 0.154506\nv 0.131434 -0.262869 0.404506\nv 0.425324 -0.262868 0.000000\nv 0.212661 -0.425327 -0.154506\nv -0.262865 -0.425326 0.000000\nv -0.344095 -0.262868 0.249998\nv -0.081228 -0.425327 -0.249998\nv -0.344095 -0.262868 -0.249998\nv 0.131434 -0.262869 -0.404506\nv 0.475529 0.000000 0.154506\nv 0.475529 0.000000 -0.154506\nv 0.000000 0.000000 0.500000\nv 0.293893 0.000000 0.404508\nv -0.475529 0.000000 0.154506\nv -0.293893 0.000000 0.404508\nv -0.293893 0.000000 -0.404508\nv -0.475529 0.000000 -0.154506\nv 0.293893 0.000000 -0.404508\nv 0.000000 0.000000 -0.500000\nv 0.344095 0.262868 0.249998\nv -0.131434 0.262869 0.404506\nv -0.425324 0.262868 0.000000\nv -0.131434 0.262869 -0.404506\nv 0.344095 0.262868 -0.249998\nv 0.081228 0.425327 0.249998\nv 0.262865 0.425326 0.000000\nv -0.212661 0.425327 0.154506\nv -0.212661 0.425327 -0.154506\nv 0.081228 0.425327 -0.249998\ns off\nusemtl kinOcean\nf 1 14 13\nf 2 14 16\nf 1 13 18\nf 1 18 20\nf 1 20 17\nf 2 16 23\nf 3 15 25\nf 4 19 27\nf 5 21 29\nf 6 22 31\nf 2 23 26\nf 3 25 28\nf 4 27 30\nf 5 29 32\nf 6 31 24\nf 7 33 38\nf 8 34 40\nf 9 35 41\nusemtl kinPole\nf 10 36 42\nf 11 37 39\nf 39 42 12\nf 39 37 42\nf 37 10 42\nf 42 41 12\nf 42 36 41\nf 36 9 41\nf 41 40 12\nf 41 35 40\nf 35 8 40\nf 40 38 12\nusemtl kinContinent\nf 40 34 38\nf 34 7 38\nf 38 39 12\nf 38 33 39\nf 33 11 39\nf 24 37 11\nf 24 31 37\nf 31 10 37\nf 32 36 10\nf 32 29 36\nf 29 9 36\nf 30 35 9\nf 30 27 35\nf 27 8 35\nf 28 34 8\nf 28 25 34\nf 25 7 34\nusemtl kinOcean\nf 26 33 7\nf 26 23 33\nf 23 11 33\nf 31 32 10\nf 31 22 32\nf 22 5 32\nf 29 30 9\nf 29 21 30\nf 21 4 30\nf 27 28 8\nf 27 19 28\nf 19 3 28\nf 25 26 7\nf 25 15 26\nf 15 2 26\nusemtl kinContinent\nf 23 24 11\nf 23 16 24\nf 16 6 24\nf 17 22 6\nf 17 20 22\nf 20 5 22\nf 20 21 5\nf 20 18 21\nf 18 4 21\nf 18 19 4\nusemtl kinPole\nf 18 13 19\nf 13 3 19\nf 16 17 6\nf 16 14 17\nf 14 1 17\nf 13 15 3\nf 13 14 15\nf 14 2 15\n";; 
	let explosion_obj = "# Blender v2.79 (sub 0) OBJ File: ''\n# www.blender.org\nv 0.000000 -0.500000 0.000000\nv 0.361800 -0.223607 0.262860\nv -0.138193 -0.223607 0.425320\nv -0.447212 -0.223607 0.000000\nv -0.138193 -0.223607 -0.425320\nv 0.361800 -0.223607 -0.262860\nv 0.138193 0.223607 0.425320\nv -0.361800 0.223607 0.262860\nv -0.361800 0.223607 -0.262860\nv 0.138193 0.223607 -0.425320\nv 0.447212 0.223607 0.000000\nv 0.000000 0.500000 0.000000\nusemtl explosionMaterial\ns off\nf 1 2 3\nf 2 1 6\nf 1 3 4\nf 1 4 5\nf 1 5 6\nf 2 6 11\nf 3 2 7\nf 4 3 8\nf 5 4 9\nf 6 5 10\nf 2 11 7\nf 3 7 8\nf 4 8 9\nf 5 9 10\nf 6 10 11\nf 7 11 12\nf 8 7 12\nf 9 8 12\nf 10 9 12\nf 11 10 12\n";; 
	let cokpit_0_obj = "# Blender v2.79 (sub 0) OBJ File: 'cokpit4.blend'\n# www.blender.org\nmtllib cokpit_0.mtl\nv 0.412485 0.000615 -0.273131\nv 0.337485 0.000615 -0.273131\nv 0.299985 0.000615 -0.305607\nv 0.206235 0.357851 -0.273131\nv 0.168735 0.292899 -0.273131\nv 0.149985 0.260423 -0.305606\nv -0.206265 0.357851 -0.273131\nv -0.168765 0.292899 -0.273131\nv -0.150015 0.260423 -0.305606\nv 0.159626 0.370895 0.210152\nv 0.159626 0.370895 -0.272348\nv 0.171358 0.296818 0.210152\nv 0.245435 0.308551 0.210152\nv 0.171358 0.296818 -0.272348\nv 0.245435 0.308551 -0.272348\nusemtl None\ns off\nf 4 2 1\nf 5 3 2\nf 4 8 5\nf 5 9 6\nf 4 5 2\nf 5 6 3\nf 4 7 8\nf 5 8 9\nf 15 12 14\nf 14 10 11\nf 15 13 12\nf 14 12 10\n";; 
	let cokpit_1_obj = "# Blender v2.79 (sub 0) OBJ File: 'cokpit4.blend'\n# www.blender.org\nmtllib cokpit_1.mtl\nv 0.336905 0.001683 0.210709\nv 0.388055 0.056535 0.210709\nv 0.336905 0.001683 -0.271791\nv 0.388055 0.056535 -0.271791\nv 0.391757 -0.049467 0.210709\nv 0.391757 -0.049467 -0.271791\nusemtl None\ns off\nf 2 3 1\nf 6 1 3\nf 2 4 3\nf 6 5 1\n";; 
	let cokpit_2_obj = "# Blender v2.79 (sub 0) OBJ File: 'cokpit4.blend'\n# www.blender.org\nmtllib cokpit_2.mtl\nv 0.412485 0.000615 -0.273131\nv 0.337485 0.000615 -0.273131\nv 0.299985 0.000615 -0.305607\nv 0.206235 -0.356620 -0.273131\nv 0.168735 -0.291668 -0.273131\nv 0.149985 -0.259192 -0.305607\nv 0.168488 -0.292145 0.210152\nv 0.243358 -0.296560 0.210152\nv 0.168488 -0.292145 -0.272348\nv 0.243358 -0.296560 -0.272348\nv 0.164073 -0.367014 0.210152\nv 0.164073 -0.367014 -0.272348\nusemtl None\ns off\nf 1 5 4\nf 2 6 5\nf 1 2 5\nf 2 3 6\nf 8 9 7\nf 12 7 9\nf 8 10 9\nf 12 11 7\n";; 
	let cokpit_3_obj = "# Blender v2.79 (sub 0) OBJ File: 'cokpit4.blend'\n# www.blender.org\nmtllib cokpit_3.mtl\nv -0.412515 0.000615 -0.273131\nv -0.337515 0.000615 -0.273131\nv -0.300015 0.000615 -0.305607\nv -0.206265 -0.356620 -0.273131\nv -0.168765 -0.291668 -0.273131\nv -0.150015 -0.259192 -0.305607\nv 0.206235 -0.356620 -0.273131\nv 0.168735 -0.291668 -0.273131\nv 0.149985 -0.259192 -0.305607\nv -0.245374 -0.304105 0.210152\nv -0.171298 -0.292373 0.210152\nv -0.245374 -0.304105 -0.272348\nv -0.171298 -0.292373 -0.272348\nv -0.159565 -0.366449 0.210152\nv -0.159565 -0.366449 -0.272348\nusemtl None\ns off\nf 4 2 1\nf 5 3 2\nf 7 5 4\nf 8 6 5\nf 4 5 2\nf 5 6 3\nf 7 8 5\nf 8 9 6\nf 11 12 10\nf 13 14 15\nf 11 13 12\nf 13 11 14\n";; 
	let cokpit_4_obj = "# Blender v2.79 (sub 0) OBJ File: 'cokpit4.blend'\n# www.blender.org\nmtllib cokpit_4.mtl\nv -0.206265 0.357851 -0.273131\nv -0.168765 0.292899 -0.273131\nv -0.150015 0.260423 -0.305606\nv -0.412515 0.000615 -0.273131\nv -0.337515 0.000615 -0.273131\nv -0.300015 0.000615 -0.305607\nv -0.391945 0.051535 0.210709\nv -0.391945 0.051535 -0.271791\nv -0.388243 -0.054467 0.210709\nv -0.337093 0.000385 0.210709\nv -0.388243 -0.054467 -0.271791\nv -0.337093 0.000385 -0.271791\nusemtl None\ns off\nf 1 5 2\nf 5 3 2\nf 1 4 5\nf 5 6 3\nf 12 9 11\nf 8 10 12\nf 12 10 9\nf 8 7 10\n";; 
	let cokpit_5_obj = "# Blender v2.79 (sub 0) OBJ File: 'cokpit4.blend'\n# www.blender.org\nmtllib cokpit_5.mtl\nv -0.164142 0.368501 0.210152\nv -0.164142 0.368501 -0.272348\nv -0.243427 0.298046 0.210152\nv -0.168557 0.293631 0.210152\nv -0.243427 0.298046 -0.272348\nv -0.168557 0.293631 -0.272348\nusemtl None\ns off\nf 6 3 5\nf 2 4 6\nf 6 4 3\nf 2 1 4\n";; 

	(*les .obj sont convertis en mesh*)
	let get_bombarder_mesh faceBuffer camera = create_mesh_from_obj bombarder_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. 0. 0. bombarderRadius bombarderRadius bombarderRadius) {lightDirection = get_nul_3D (); lightColor = create_color 0 0 0};;
	let get_hunter_mesh faceBuffer camera = create_mesh_from_obj hunter_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. 0. 0. hunterRadius hunterRadius hunterRadius) {lightDirection = get_nul_3D (); lightColor = create_color 0 0 0};;
	let get_destroyer_mesh faceBuffer camera =  create_mesh_from_obj destroyer_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. 0. 0. destroyerRadius destroyerRadius destroyerRadius) {lightDirection = get_nul_3D (); lightColor = create_color 0 0 0};;
	let get_laser_mesh faceBuffer camera = create_mesh_from_obj laser_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. 0. 0. 5. 0.25 0.25) {lightDirection = get_nul_3D (); lightColor = create_color 0 0 0};;
	let get_bombe_mesh faceBuffer camera = create_mesh_from_obj bombe_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. 0. 0. bombeRadius bombeRadius bombeRadius) {lightDirection = get_nul_3D (); lightColor = create_color 0 0 0};;
	let get_alpha_42_phb_mesh faceBuffer camera = create_mesh_from_obj alpha_42_phb_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. 0. 0. (2.*.sunRadius) (2.*.sunRadius) (2.*.sunRadius)) {lightDirection = get_nul_3D (); lightColor = create_color 0 0 0};;(*l'etoile*)
	let get_kin_mesh faceBuffer camera = create_mesh_from_obj kin_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. 0. 0. (2.*.planetteRadius) (2.*.planetteRadius) (2.*.planetteRadius)) {lightDirection = get_nul_3D (); lightColor = create_color 0 0 0};; (*la planette Kïn*)
	let get_explosion_mesh faceBuffer camera = create_mesh_from_obj explosion_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. 0. 0. 1. 1. 1.) {lightDirection = get_nul_3D (); lightColor = create_color 0 0 0};;
	let get_cokpit_0_mesh faceBuffer camera environnementLocal = create_mesh_from_obj cokpit_0_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. (3.*.pi/.2.) 0. 0.25 0.25 0.75) environnementLocal;;
	let get_cokpit_1_mesh faceBuffer camera environnementLocal = create_mesh_from_obj cokpit_1_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. (3.*.pi/.2.) 0. 0.25 0.25 0.75) environnementLocal;;
	let get_cokpit_2_mesh faceBuffer camera environnementLocal = create_mesh_from_obj cokpit_2_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. (3.*.pi/.2.) 0. 0.25 0.25 0.75) environnementLocal;;
	let get_cokpit_3_mesh faceBuffer camera environnementLocal = create_mesh_from_obj cokpit_3_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. (3.*.pi/.2.) 0. 0.25 0.25 0.75) environnementLocal;;
	let get_cokpit_4_mesh faceBuffer camera environnementLocal = create_mesh_from_obj cokpit_4_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. (3.*.pi/.2.) 0. 0.25 0.25 0.75) environnementLocal;;
	let get_cokpit_5_mesh faceBuffer camera environnementLocal = create_mesh_from_obj cokpit_5_obj faceBuffer camera (get_moveMatrix (get_nul_3D ()) 0. (3.*.pi/.2.) 0. 0.25 0.25 0.75) environnementLocal;;

	let merge_array a1 a2 = 
	(*permet de fusionner 2 array*)
		let n1 = Array.length a1 and
			n2 = Array.length a2 in
		let n = n1 + n2 in
		let a = Array.make n a1.(0) in
		for i = 0 to n1 - 1 do 
			a.(i) <- a1.(i)
		done;
		for i = 0 to n2 - 1 do
			a.(i + n1) <- a2.(i)
		done;
		a;;

	let get_cokpit_mesh cokpitParts faceBuffer camera =
	(*fusionnes les diferents mesh représentant le cokpit du tourist (votre vaisseau) en foncyion de la vie de se dernier*)
		let environnementLocal = {lightDirection = get_nul_3D (); lightColor = create_color 0 0 0} in
		let oldMesh = ref (new mesh [||] [||] faceBuffer environnementLocal) in
		let parts = ref cokpitParts in
		while !parts <> [] do
			let h::t = !parts in
			parts := t;
			let newMesh = 
				match h with
				|0 -> get_cokpit_0_mesh faceBuffer camera environnementLocal
				|1 -> get_cokpit_1_mesh faceBuffer camera environnementLocal
				|2 -> get_cokpit_2_mesh faceBuffer camera environnementLocal
				|3 -> get_cokpit_3_mesh faceBuffer camera environnementLocal
				|4 -> get_cokpit_4_mesh faceBuffer camera environnementLocal
				|_ -> get_cokpit_5_mesh faceBuffer camera environnementLocal
			in
			let npts = newMesh#get_points () and
				nfcs = newMesh#get_faces () and
				opts = !oldMesh#get_points () and
				ofcs = !oldMesh#get_faces () in
			oldMesh := new mesh (merge_array npts opts) (merge_array nfcs ofcs) faceBuffer environnementLocal
		done;
		!oldMesh;;

"IA";;
	let get_pitch_roll_with_target reper target = 
		(*target est le vecteur partant du centre du repère vers la cible*)
		(*donne les angle de rotation a appliquer a un vaisseau pour qu'il se dirige vers sa cible*)
		let y = ref ((scalar reper.e2 target) /. target#get_norme ()) and
			z = ref ((scalar reper.e3 target) /. target#get_norme ()) in
		!y, !z;;

	let player_controls (rep : repereCinetique) = 
		(*li les control du joueur*)
		let sys = Graphics.wait_next_event [Graphics.Poll] in
		let fire = Graphics.button_down () || sys.button in
		let k = Graphics.key_pressed () && Graphics.read_key () = ' ' in
		let roll = (tf (sys.mouse_x - (Graphics.size_x ())/2)) /. tf (Graphics.size_x ()) and
			pitch = (tf (sys.mouse_y - (Graphics.size_y ())/2)) /. tf (Graphics.size_y ()) in
		{pitch = pitch *. abs_float pitch; roll = roll *. abs_float roll; fire = fire; speedUp = k};;

	let born x a b = 
		(*borne une valeur x entre a et b*)
		if a < x then if x < b then x else b else a;;

	let get_IA shootDistance accelerateDistance = 
	(*permet de créé les IA*)
		let iA (rep : repereCinetique) (target : vector3D) =
		let pitch, roll = get_pitch_roll_with_target rep.rep target in
		let pi = born pitch (-.1.) 1. and
			ro = born roll (-.1.) 1. and
			fire = target#get_norme () < shootDistance and
			speedUp = target#get_norme () > accelerateDistance in
		{pitch = pi; roll = ro; fire = fire; speedUp = speedUp} 
		in iA;;

	let hunterIA = get_IA hunterShootDistance hunterAccelerateDistance;;
	let bombarderIA = get_IA bombarderShootDistance bombarderAccelerateDistance;;
	let bombeIA = get_IA min_float max_float;;
	let destroyerIA = get_IA destroyerShootDistance destroyerAccelerateDistance;;


"entities";;
	class virtual entity (mesh : mesh) (repCin : repereCinetique) camera environnement hp (attack : int) phisic_data = object (self)
	(*represente une entite dans l'éspace 3D*)
		val fullHp = tf hp
		val mutable hp = hp
		val mutable selfMatrix = get_changement_de_base_matrix repCin.rep
		val mutable positionInFrontCamera = repCin.rep.o#copy3D ()
		val mutable isProjected = false
		val mutable repCin = repCin
		val phisic_data = phisic_data
		val attack = attack
		val camera = camera
		val mutable mesh = mesh
		val environnement = environnement

		method get_percent_of_life () = 
			(*renvoie le pourcentage de vie de l'entité*)
			tf hp /. fullHp
		method virtual update : unit -> unit 
		method private is_in_front_camera () = 
		(*vrai si l'entité apparait a l'écran, permer d'avorter les calcul inutiles*)
			let d = positionInFrontCamera#get_norme () and
				y = positionInFrontCamera#get_y () and
				z = positionInFrontCamera#get_z () in
			positionInFrontCamera#get_x () +. phisic_data.radius > 0. &&
				(abs_float z -.phisic_data.radius)/.d < camera.cosO &&
				(abs_float y -.phisic_data.radius)/.(d*.camera.a) < camera.cosO
		method private move_in_front_camera (movesMatrixs : movesMatrixs) = 
		(*permet de faire passer les points (initialement dans b-obj) dans b-cam en prenant B(bc->b-cam)*)
			let v = matrix_add  movesMatrixs.translation repCin.rep.o in
			positionInFrontCamera <- convert_vector3D (matrix_product movesMatrixs.rotation v);
			if self#is_in_front_camera () then begin
				(mesh#get_environnementLocal()).lightDirection <- environnement#get_light_in_camera positionInFrontCamera;
				(mesh#get_environnementLocal()).lightColor <- environnement#get_light_color ();
				mesh#move_opty selfMatrix movesMatrixs
			end
		method private reset () = 
		(*renitialise les points et les faces*)
			mesh#reset ();
			isProjected <- false;
		method private put_faces_in_buffer () = 
		(*...*)
			if self#is_in_front_camera () then mesh#put_faces_in_buffer ();
		method private inherit_render () = () (*permet d'ajouter un rendu spécifiquepour les classe enfant*)
		method load_faces cameraMatrix = 
		(*charge les faces dans le faceBuffer*)
			self#reset ();
			self#move_in_front_camera cameraMatrix;
			self#put_faces_in_buffer ();
			self#inherit_render ()
		method cinetique_update speedUp = 
		(*calcul l'évolution cinetique (position et vitesse) de l'entité, durant la phase d'update*)
			let v = repCin.rep.e1#copy3D () in
			let speed = if speedUp then ref (repCin.v +. phisic_data.motorPropulsion) else ref repCin.v in
			speed := min (max phisic_data.minSpeed (!speed *. phisic_data.frottement)) phisic_data.maxSpeed;
			v#normalize ();
			v#dot (dt *. !speed);
			repCin.v <- !speed;
			repCin.rep.o <- (vector_add repCin.rep.o v);
			selfMatrix <- get_changement_de_base_matrix repCin.rep;
		method private set_isProjected state = isProjected <- state
		method is_in_live () = hp > 0
		method get_colision_data () = repCin.rep.o, phisic_data.radius, phisic_data.identifiant
		method get_damage damage = hp <- hp - damage
		method get_attack () = attack
		method get_repCin () = repCin
		method get_phisic_data () = phisic_data
		method get_mesh () = mesh
		method set_mesh mesh_ = mesh <- mesh_
		method get_x () = repCin.rep.o#get_x ()
		method get_y () = repCin.rep.o#get_y ()
		method get_z () = repCin.rep.o#get_z ()
		method get_positionInFrontCamera () = positionInFrontCamera
		method die () = ()(*permet au classes enfant de faire une action particuliere a la mort*)
		method destruct () = hp <- 0
	end;;

	class entityBuffer = object (self)
	(*gestionnaire d'entité, s'occupe de les actualiser et de les afficher*)
		val mutable buffer = []
		val mutable addedEntities = []

		method load_faces movesMatrixs = 
		(*charge les faces dans le faceBuffer*)
			foreach_list buffer 
				(fun entity -> entity#load_faces movesMatrixs)
		method update () = 
			(*On update toutes les entités du jeu*)
			foreach_list buffer
				(fun entity -> entity#update ());
			(*On calcule les collisions*)
			let temp1 = ref buffer in
			while !temp1 <> [] do
				let first::other = !temp1 in
				temp1 := other;
				let temp2 = ref other in
				while !temp2 <> [] do
					let second::t = !temp2 in
					temp2 := t;
					let p1, r1 , i1= first#get_colision_data () and
						p2, r2 , i2= second#get_colision_data () in
					let p = vector_sub p1 p2 in
					if i1 <> i2 && p#get_norme () < r1+.r2 then begin
						first#get_damage (second#get_attack ());
						second#get_damage (first#get_attack ());
					end
				done
			done;
			(*On supprime les entités mortes*)
			let inLives = ref [] in
			foreach_list buffer
				(fun entity -> 
					if entity#is_in_live () then inLives := entity::!inLives
					else entity#die ());
			buffer <- !inLives;
			(*On ajoute les entités créées durant ce tour de calcul*)
			foreach_list addedEntities (fun entity -> buffer <- entity::buffer);
			addedEntities <- []
		method add_entity (entitie : entity) = addedEntities <- entitie::addedEntities;
	end;;

	class laser repCin camera facesBuffer environnement identifiant = object (self)
	(*permet de créé des laser*)
		inherit entity (get_laser_mesh facesBuffer camera) repCin camera environnement 1 10 {minSpeed = laserSpeed; maxSpeed = laserSpeed; radius = laserRadius; motorPropulsion = 1.; frottement = 1.; identifiant = identifiant} as super
		val timeLimite = ref laserTimeLimit (*detruit le laser au bout d'un certain temps pour ne pas surcharger le jeu*)

		method update () = 
			super#cinetique_update false;
			decr timeLimite;
			if !timeLimite = 0 then super#destruct ();
	end;;

	class explosion repCin camera faceBuffer environnement radius entitie duration expension phisic_data = object (self)
	(*permet de créé des explosion*)
		inherit entity (get_explosion_mesh faceBuffer camera) repCin camera environnement max_int explosionAttaque phisic_data as super
		val entitie = entitie
		val duration = duration
		val expension = expension
		val lifeTime = ref 0

		method is_in_live () = 
			(*suicide l'explosion au bout d'un certain temps*)
			!lifeTime < duration
		method update () = 
			incr lifeTime;
			let rep = super#get_repCin () in
			rep.rep.e1#dot expension;
			rep.rep.e2#dot expension;
			rep.rep.e3#dot expension;
			(super#get_phisic_data ()).radius <- (super#get_phisic_data ()).radius *. expension;
			super#cinetique_update true;
		initializer
			let rep = super#get_repCin () in
				rep.rep.e1#dot radius;
				rep.rep.e2#dot radius;
				rep.rep.e3#dot radius;
	end;;

	
		
	class virtual spaceShip mesh repereCinetique entityBuffer environnement phisic_data camera hp attack iconeBuffer nbPointsIcone iconeRadius = object (self)
	(*permet de créé des vaisseaux spacieaux*)
		inherit entity mesh repereCinetique camera environnement hp attack phisic_data as super
		val entityBuffer : entityBuffer = entityBuffer
		val weaponCouldown = ref 0

		method private create_repCinWeapon de1 de2 de3 = 
		(*permet de transmettre une copie du repère du vaisseau, à une autre position de l'espace pour que l'arme n'explose pas en apparaissant sur le vaisseau*)
			let rep = copy_repereCinetique (super#get_repCin ()) in
			let e1_ = rep.rep.e1#copy3D () and
				e2_ = rep.rep.e2#copy3D () and
				e3_ = rep.rep.e3#copy3D () in
			e1_#dot de1;
			e2_#dot de2;
			e3_#dot de3;
			let p = (vector_add e3_ (vector_add e2_ (vector_add rep.rep.o e1_))) in
			rep.rep.o <- p;
			rep
		method virtual private fire : int ref -> unit
	end;;

	class virtual empireShip mesh repereCinetique entityBuffer environnement phisic_data camera strategie (target : entity) hp attack iconeBuffer nbPointsIcone iconeRadius sensibilite = object (self)
	(*représente les vaisseau ennemis, avec leurs IA, et leurs icones*)
		val icone = new icone nbPointsIcone camera iconeRadius
		inherit spaceShip mesh repereCinetique entityBuffer environnement phisic_data camera hp attack iconeBuffer nbPointsIcone iconeRadius as super
		val strategie = strategie
		val target = target
		val sensibilite = sensibilite
		val mutable control = {pitch = 0.; roll = 0.; fire = false; speedUp = false}
		val reflexe = ref 0
		val mutable nextReflexe = 40 (*permet de donner un temps de réaction au IA*)

		method update () = 
			incr weaponCouldown;
			incr reflexe;
			if !reflexe > nextReflexe then begin
				control <- strategie repereCinetique  (vector_sub (target#get_repCin ()).rep.o (super#get_repCin ()).rep.o);
				reflexe := 0;
				nextReflexe <- 40 + Random.int 40
			end;
			let mat = ship_rotation (control.pitch*.sensibilite) (control.roll*.sensibilite) in
			rotation_repere repereCinetique.rep mat;
			super#cinetique_update control.speedUp;
			if control.fire then self#fire weaponCouldown
		method private set_icone_color () =
			let percentHp = super#get_percent_of_life () in
	 		if percentHp < 0.4 then icone#set_color iconeColorNoLife
	 		else if percentHp < 0.7 then icone#set_color iconeColorMidLife
			else icone#set_color iconeColorFullLife
		method private inherit_render () = 
			let position = super#get_positionInFrontCamera () in
			let px = ((position#get_z ())*.camera.ad/. abs_float (position#get_x ()) +. 1.) *. tf camera.size_x /. 2. and
				py = ((position#get_y ())*.camera.d/. abs_float (position#get_x ()) +. 1.) *. tf camera.size_y /. 2. in
	 		let d = if position#get_x () < 0. then -.(position#get_norme ()) else position#get_norme () in
	 		icone#set_position px py d;
	 		self#set_icone_color ()
	 	method die () =	
			icone#die ();
			let rep = copy_repereCinetique (super#get_repCin ()) in
			let e = new explosion rep camera ((super#get_mesh ())#get_faceBuffer ()) environnement (super#get_phisic_data ()).radius self 15 1.15 (super#get_phisic_data ()) in
			entityBuffer#add_entity e;
		 initializer iconeBuffer#add_icone icone
	end;;

	class bombe repCin camera faceBuffer environnement entityBuffer identifiant target iconeBuffer = object (self)
	(*permet au ennemi de créé des bombes*)
		inherit empireShip (get_bombe_mesh faceBuffer camera) repCin entityBuffer environnement  {minSpeed = bombeMinSpeed; maxSpeed = bombeMaxSpeed; radius = bombeRadius; motorPropulsion = 1.; frottement = 1.; identifiant = identifiant} camera bombeIA target bombeHP bombeAttack iconeBuffer 6 bombeIconeRadius bombeSensibilite as super
		val timeLimite = ref bombeTimeLimit

		method update () = 
			let control = bombeIA repCin  (vector_sub (target#get_repCin ()).rep.o (super#get_repCin ()).rep.o) in
			let mat = ship_rotation (control.pitch*.bombeSensibilite) (control.roll*.bombeSensibilite) in
			rotation_repere repCin.rep mat;
			super#cinetique_update control.speedUp;
			decr timeLimite;
			if !timeLimite = 0 then super#destruct ();
		method private fire weaponCouldown = () 
	end;;

	let remove_random_list l =
	(*enleve un élément d'une list*)
		let rec remove i l = match i with
			| 0 -> let h::t = l in t
			| _ -> let h::t = l in h::remove (i-1) t
		in
		let n = List.length l in
		remove (Random.int n) l;;
		
	class turist repereCinetique entityBuffer faceBuffer camera environnement iconeBuffer = object (self)
	(*votre vaisseaux*)
		inherit spaceShip (get_cokpit_mesh [0;1;2;3;4;5] faceBuffer camera) repereCinetique entityBuffer environnement (get_turist_phiscal_data ()) camera turistHP turistAttack iconeBuffer 0 0. as super
		val mutable dVector = get_nul_3D ()
		val surcautDelay = ref 0
		val mutable lifeState = 0.85
		val mutable cokpitParts = [0;1;2;3;4;5]
		val mutable control = {pitch = 0.; roll = 0.; fire = false; speedUp = false}
		val reflexe = ref 0 (*permet de ne solicité les commande que 30 fois par secondes, Graphics a du mal a donner des résultat cohérant au dela de cette fréquance... :( *)
		val nextReflexe = playerReflexe

		method private fire weaponCouldown = 
		(*vous permet de tirez des laser dans la direction de votre regard*)
			if !weaponCouldown > turistReloadTime then begin
				weaponCouldown := 0;
				let faceBuffer = (super#get_mesh ())#get_faceBuffer () and
					r = (super#get_phisic_data ()).radius in
				entityBuffer#add_entity (new laser (super#create_repCinWeapon (laserRadius +. r) (-.r/.2.) r ) camera faceBuffer environnement (super#get_phisic_data ()).identifiant);
				entityBuffer#add_entity (new laser (super#create_repCinWeapon (laserRadius +. r) (-.r/.2.) (-.r) ) camera faceBuffer environnement (super#get_phisic_data ()).identifiant);
			end
		(*puisque cet objet sera toujours placé au même endroit dans le repère de la caméra, on peut modifier les fonctions de changement de base pour gagner du temps*)
		method private is_in_front_camera () = true
		method private move_in_front_camera (movesMatrixs : movesMatrixs) = 
			(mesh#get_environnementLocal()).lightDirection <- environnement#get_light_in_camera positionInFrontCamera;
			(mesh#get_environnementLocal()).lightColor <- environnement#get_light_color ();
			(super#get_mesh ())#translate dVector
		method private reset () = 
		(*renitialise le vaisseau*)
			super#set_isProjected false;
			(super#get_mesh ())#reset ();
		method private update_cokpit () = 
		(*enleve des partie du cokpit si besoin*)
			if super#get_percent_of_life () < lifeState then begin
				cokpitParts <- remove_random_list cokpitParts;
				lifeState <- lifeState -. 0.15;
				super#set_mesh (get_cokpit_mesh cokpitParts ((super#get_mesh ())#get_faceBuffer ()) camera)
			end
		method update () = 
		(*calcul de jeu*)
			self#update_cokpit ();
			incr weaponCouldown;
			incr surcautDelay;
			incr reflexe;
			if !reflexe >= nextReflexe then begin
				(*regardes les controls du joueur*)
				control <- player_controls repereCinetique;
				reflexe := 0
			end;
			(* applique les controls *)
			let mat = ship_rotation (control.pitch*.turisteSensibilite) (control.roll*.turisteSensibilite) in
			rotation_repere repereCinetique.rep mat;
			super#cinetique_update control.speedUp;
			if control.fire then self#fire weaponCouldown;
			if !surcautDelay > surcaultCouldown then begin
				(*deplace legerement de mesh afin de donner une impresion de vitesse*)
				surcautDelay := 0;
				let v = (super#get_repCin ()).v in
				let dx = (Random.float surcaulImpact -. surcaulImpact/.2.) *. v and
					dy = (Random.float surcaulImpact -. surcaulImpact/.2.) *. v and
					dz = (Random.float surcaulImpact -. surcaulImpact/.2.) *. v in
				dVector <- new vector3D dx dy dz
			end
		method die () =	
			(*crée une explosion*)
			let rep = copy_repereCinetique (super#get_repCin ()) in
			let e = new explosion rep camera ((super#get_mesh ())#get_faceBuffer ()) environnement (super#get_phisic_data ()).radius self 15 1.15 (super#get_phisic_data ()) in
			entityBuffer#add_entity e
	end;;
	class hunter repereCinetique entityBuffer faceBuffer camera environnement iconeBuffer target = object (self)
	(*les chasseurs*)
		inherit empireShip (get_hunter_mesh faceBuffer camera) repereCinetique entityBuffer environnement (get_hunter_phiscal_data ()) camera hunterIA target hunterHP hunterAttack iconeBuffer 5 hunterIconeRadius hunterSensibilite as super

		method private fire weaponCouldown = 
			if !weaponCouldown > hunterReloadTime then begin
				weaponCouldown := 0;
				let faceBuffer = (super#get_mesh ())#get_faceBuffer () and
					r = (super#get_phisic_data ()).radius in
				entityBuffer#add_entity (new laser (super#create_repCinWeapon (laserRadius +. r) (-.r/.2.) r ) camera faceBuffer environnement (super#get_phisic_data ()).identifiant);
				entityBuffer#add_entity (new laser (super#create_repCinWeapon (laserRadius +. r) (-.r/.2.) (-.r) ) camera faceBuffer environnement (super#get_phisic_data ()).identifiant);
			end
	end;;

	class bombarder repereCinetique entityBuffer faceBuffer camera environnement iconeBuffer target = object (self)
	(*les bombardiers*)
		inherit empireShip (get_bombarder_mesh faceBuffer camera) repereCinetique entityBuffer environnement (get_bombarder_phiscal_data ()) camera bombarderIA target bombarderHP bombarderAttack iconeBuffer 4 bombarderIconeRadius bombarderSensibilite as super

		method private fire weaponCouldown = 
			if !weaponCouldown > bombarderReloadTime then begin
				weaponCouldown := 0;
				let faceBuffer = (super#get_mesh ())#get_faceBuffer () and
					r = (super#get_phisic_data ()).radius in
				entityBuffer#add_entity (new bombe (super#create_repCinWeapon (bombeRadius +. r) 0. 0. ) camera faceBuffer environnement entityBuffer (super#get_phisic_data ()).identifiant target iconeBuffer);
			end
	end;;

	class destroyer repereCinetique entityBuffer faceBuffer camera environnement iconeBuffer planette (player : entity) = object (self)
	(*les destroyer Imperial (même si il n'y en a toujours qu'un seul invoquer par partie, il est trés facil d'en mettre pllus)*)
		inherit empireShip (get_destroyer_mesh faceBuffer camera) repereCinetique entityBuffer environnement (get_destroyer_phiscal_data ()) camera destroyerIA planette destroyerHP destroyerAttack iconeBuffer 3 destroyerIconeRadius destroyerSensibilite as super
		val player = player
		val waveNumber = Array.length waves
		val mutable currentWave = 0
		val mutable curentWaveLifeToLose = 1.

		method private create_ship_rep () =
			(*invoque in chasseur ou un bombardier dans la direction du joueur (afin que se dernier ne traverse pas le setroyer pour se raprocher du joueur)*)
			let rep = copy_repereCinetique (super#get_repCin ()) in
			let v = (vector_sub rep.rep.o (player#get_repCin ()).rep.o) in
			v#normalize ();
			v#dot (-.destroyerRadius  -. (max hunterRadius bombarderRadius) -. 1.);
			rep.rep.o <- (vector_add v rep.rep.o);
			rep 
		method update () = 
			(*les calcul de jeu, si la vie a trop diminué, invique un ennemie*)
			incr weaponCouldown;
			let control = strategie repereCinetique  (vector_sub (target#get_repCin ()).rep.o (super#get_repCin ()).rep.o) in
			let mat = ship_rotation (control.pitch*.sensibilite) (control.roll*.sensibilite) in
			rotation_repere repereCinetique.rep mat;
			super#cinetique_update control.speedUp;
			if control.fire then self#fire weaponCouldown;
			if currentWave <> Array.length waves then begin
				let content, lifeToLose = waves.(currentWave) in
				if super#get_percent_of_life () +. lifeToLose < curentWaveLifeToLose then begin
					currentWave <- currentWave + 1;
					curentWaveLifeToLose <- curentWaveLifeToLose -. lifeToLose;
					match content with
						|Hunter -> entityBuffer#add_entity (new hunter (self#create_ship_rep ()) entityBuffer faceBuffer camera environnement iconeBuffer player)
						|_ -> entityBuffer#add_entity (new bombarder (self#create_ship_rep ()) entityBuffer faceBuffer camera environnement iconeBuffer player)
				end
			end
		method private fire weaponCouldown = 
		(*invoque des bombe pour detruire la planette*)
			if !weaponCouldown > destroyerReloadTime then begin
				weaponCouldown := 0;
				let faceBuffer = (super#get_mesh ())#get_faceBuffer () and
					r = (super#get_phisic_data ()).radius in
				entityBuffer#add_entity (new bombe (super#create_repCinWeapon (bombeRadius +. r) 0. 0. ) camera faceBuffer environnement entityBuffer (super#get_phisic_data ()).identifiant planette iconeBuffer);
				entityBuffer#add_entity (new bombe (super#create_repCinWeapon (bombeRadius +. r) 0. (-.r) ) camera faceBuffer environnement entityBuffer (super#get_phisic_data ()).identifiant planette iconeBuffer);
				entityBuffer#add_entity (new bombe (super#create_repCinWeapon (bombeRadius +. r) 0. r ) camera faceBuffer environnement entityBuffer (super#get_phisic_data ()).identifiant planette iconeBuffer);
			end
	end;;

	class astre repCin camera faceBuffer mesh environnement hp attack radius rotationSpeed = object (self)
	(*les diferents astres (planette et Alpha_42_phb)*)
		inherit entity mesh repCin camera environnement hp attack {minSpeed = 0.; maxSpeed = 0.; radius = radius; motorPropulsion = 0.; frottement = 0.; identifiant = get_id ()} as super
		val mutable angle = 0.
		val rotationSpeed = rotationSpeed

		method update () = 
			(*fait tourner l'astre*)
			let rot = get_rotationMatrix rotationSpeed 0. 0. in
			rotation_repere (super#get_repCin ()).rep rot;
			super#cinetique_update false;
	end;;

	class alpha_42_phb repCin camera faceBuffer environnement = object (self)
		inherit astre repCin camera faceBuffer (get_alpha_42_phb_mesh faceBuffer camera) environnement sunHP sunAttack sunRadius sunRotationSpeed as super
	end;;
	class kin repCin camera faceBuffer environnement = object (self)
		inherit astre repCin camera faceBuffer (get_kin_mesh faceBuffer camera) environnement planetteHP planetteAttack planetteRadius planetteRotationSpeed as super
	end;;

class game camera repereCamera = object (self)
	(*le chef d'orchestre du jeu, gere alternativement les updates et les render des objet concernés*)
	val mutable masterEntities = [||] (*[|Player; planette; Destroyer|]*)
 	val faceBuffer = new faceBuffer
	val entityBuffer = new entityBuffer
	val iconeBuffer = new iconeBuffer
	val starBuffer = new starBuffer nbStars camera repereCamera
	val camera = camera
	val repereCamera = repereCamera
	val environnement = new environnement (new vector3D 2001. 100. (-.200.)) (create_color 200 150 130)
	val mutable haveWin = false
	val mutable haveLose = false
	val endDelay = ref 0 (*permet de laisser quelques secondes au moins l'écran de fin*)
	
	method private create_random_repCin x y z v = 
	(*genere un repere cinétique aleatoire a une position et une vitesse donné*)
		let repCin = {rep = get_random_repere x y z; v = v} in
		repCin
	method private win () = 
	(*affiche le message de victoire*)
		let offset = min (camera.size_x/2) camera.size_y /5 in
		Graphics.moveto offset (camera.size_y - 3*offset);
		Graphics.set_text_size offset;
		Graphics.set_color (Graphics.rgb 200 200 200);
		Graphics.draw_string "Félicitation!";
	method private lose () = 
	(*affiche le message de defaite*)
		let offset = min (camera.size_x) camera.size_y /5 in
		Graphics.moveto (2*offset) (camera.size_y - 3*offset);
		Graphics.set_text_size offset;
		Graphics.set_color (Graphics.rgb 200 100 70);
		Graphics.draw_string "Game Over";
	method update () = 
	(*opere les calcul de jeu*)
		if !endDelay > 0 then decr endDelay;
		entityBuffer#update ();
		iconeBuffer#update ();
		if not haveWin && not (masterEntities.(2)#is_in_live ()) then begin haveWin <- true; endDelay := 120 end;
		if not haveLose && not (masterEntities.(0)#is_in_live () && masterEntities.(1)#is_in_live ()) then begin haveLose <- true; endDelay := 120 end;
		(haveWin || haveLose) && !endDelay <= 0 && Graphics.button_down ()
	method render () = 
		(*opere les calcul de rendu*)
		let cameraMatrix = get_camera_base repereCamera in
		starBuffer#render cameraMatrix.rotation;
		entityBuffer#load_faces cameraMatrix;
		environnement#move_light_in_camera cameraMatrix.rotation;
		faceBuffer#render ();
		faceBuffer#reset ();
		iconeBuffer#render ();
		if haveWin && not haveLose then self#win ();
		if haveLose then self#lose ();
	initializer
		(*initialise le jeu*)
		let planette = new kin (self#create_random_repCin 500. (-.50.) 20. 0.) camera faceBuffer environnement and
			player = new turist {rep = repereCamera; v = 0.} entityBuffer faceBuffer camera environnement iconeBuffer in
		masterEntities <- [|player; planette; new destroyer (self#create_random_repCin (-.200.) 10. 0. 30.) entityBuffer faceBuffer camera environnement iconeBuffer planette player|];
		entityBuffer#add_entity (new alpha_42_phb (self#create_random_repCin 2000. 100. (-.200.) 0.) camera faceBuffer environnement);
		entityBuffer#add_entity masterEntities.(0);
		entityBuffer#add_entity masterEntities.(1);
		entityBuffer#add_entity masterEntities.(2)
end;;

"Display";;
	let create_windows camera = 
		(*crée la fenetre*)
		Graphics.open_graph ((string_of_int camera.size_x)^"x"^(string_of_int camera.size_y));
		Graphics.set_window_title "Évwayù";
		Graphics.auto_synchronize false;
		Graphics.set_color Graphics.black;
		Graphics.fill_rect 0 0 camera.size_x camera.size_y;
		Graphics.set_color Graphics.white;;

	let clear_windows camera = 
		(*efface l'image précédante et affiche la nouvelle*)
		Graphics.synchronize ();
		Graphics.clear_graph ();
		Graphics.auto_synchronize false;
		Graphics.set_color (Graphics.rgb 0 30 0);
		update_camera camera;
		Graphics.fill_rect 0 0 camera.size_x camera.size_y;
		Graphics.set_color Graphics.white;;

	let stats ticks frames emptyLoop startTime = 
		(*afiche les performance dans la consol (en fin de partie)*)
		let time = Unix.gettimeofday () -. startTime in
		print_string "tick/s : ";
		print_float ((tf ticks)/.time);
		print_string "\nFPS : ";
		print_float ((tf frames)/.time);
		print_string "\nempty_loop/s: ";
		print_float ((tf emptyLoop )/.time);
		print_string "\n";;

	let display_screen ticks frames emptyLoop = 
		(*affiche les performance dans le titre de la fenetre (en cour de partie)*)
		let s n = string_of_int (int_of_float n) in
		let t = " | tick : "^s (tf !ticks) and
			f = " | fps : "^s (tf !frames) and
			e = " | emptyLoop : "^s (tf !emptyLoop) in
		ticks := 0;
		frames := 0;
		emptyLoop := 0;
		Graphics.set_window_title ("Évwayù"^t^f^e);;

	let intro_txt = 
	(*texte introductif*)
	"   Bienvenue jeune recrue!\n\n   La Galaxie compte sur vous,\nvous ^tes notre dernier espoir...\n(un bien maigre espoir).\n\n   Le dernier syst`me stellaire encore libre,\nAlpha 42 phb. est menac~.\nLa plan`te K:n est le\ndernier rempart face a l|Empire.\nVotre mission est simple,\nd~truire la flotte Imp~riale.\nPour cela \nvous avez @ disposition un vaisseau\ninitialement con$u pour le tourisme...\nBonne chance!\n\n   Toutefois,\ncette navette a ~t~ quelque peu modifi~e,\nnotamment la barre horizontale du panneau de contr*le,\naussi appel~e clavier,\nvous permettra d|acc~l~rer.\nDes lasers ont aussi ~t~ install~s,\nactivables en cliquant sur le pointeur du cockpit,\nplus couramment appel~ souris.\nFinalement,\nd~placer le pointeur vous permettra de tourner sur vous-m^me.\nBon courage, vous ^tes notre dernier espoir";;
	
	let string_of_char ch = match ch with
	(*la fonction string_of_char n'étant pas implémenter, j'ai du la créé moi même. il y a bien une alternative, mais l'autre foncion (initialement présent dans Ocaml) ne convertie pas correctement les charactères spécieaux*)
		|'a'->"a"|'b'->"b"|'c'->"c"|'d'->"d"|'e'->"e"|'f'->"f"|'g'->"g"|'h'->"h"|'i'->"i"|'j'->"j"|'k'->"k"|'l'->"l"|'m'->"m"|'n'->"n"|'o'->"o"|'p'->"p"|'q'->"q"|'r'->"r"|'s'->"s"|'t'->"t"|'u'->"u"|'v'->"v"|'w'->"w"|'x'->"x"|'y'->"y"|'z'->"z"|'A'->"A"|'B'->"B"|'C'->"C"|'D'->"D"|'E'->"E"|'F'->"F"|'G'->"G"|'H'->"H"|'I'->"I"|'J'->"J"|'K'->"K"|'L'->"L"|'M'->"M"|'N'->"N"|'O'->"O"|'P'->"P"|'Q'->"Q"|'R'->"R"|'S'->"S"|'T'->"T"|'U'->"U"|'V'->"V"|'W'->"W"|'X'->"X"|'Y'->"Y"|'Z'->"Z"|' '->" "|' '->" "|'!'->"!"|'.'->"."|','->","|'|'->"'"|'-'->"-"|'1'->"1"|'2'->"2"|'3'->"3"|'4'->"4"|'5'->"5"|'6'->"6"|'7'->"7"|'8'->"8"|'9'->"9"|'0'->"0"|'('->"("|')'->")"|':'->"ï"|'$'->"ç"|'^'->"ê"|'~'->"é"|'`'->"è"|'@'->"à"|'*'->"ô"|_->"ERREUR";;

	let get_waiting_string nb = match nb with
	(*renvoie un charactere d'attente dans l'annimation a la fin du message introductif*)
		|0 -> "-"
		|1 -> "/"
		|2 -> "|"
		|_ -> "\\";;

	let get_intro nb = 
	(*renvoie les 'nb' premier charactère du texte introductif, avec si besoin ,le charactère d'attente*)
	let txt = ref [""] in
	for i = 0 to min (String.length intro_txt -1) nb do begin
		if intro_txt.[i] = '\n' then txt := ""::!txt
		else let h::t = !txt in txt := (h^string_of_char intro_txt.[i])::t
	end
	done;
	begin if nb < String.length intro_txt && nb mod 2 = 0 then let h::t = !txt in txt := (h^"_")::t; end;
	begin if nb > String.length intro_txt then txt := (get_waiting_string(nb mod 4))::!txt; end;
	List.rev !txt;;

	class intro = object (self)
	(*gestionnaire du menu introductif, fait évoluer ce menu et permet de l'afficher*)
		val time = ref 0
		val speed = delayBetweenLetters

		method update () = 
		(*fait évoluer le menu*)
			incr time;
			if !time/speed < String.length intro_txt && intro_txt.[!time/speed] <> '\n' then time := !time + timePerLetter-1
		method render camera = 
		(*affiche le menu en fonction des dimensions de l'écran*)
		let offset = min camera.size_x camera.size_y /30 in
		Graphics.moveto offset (camera.size_y - 2*offset);
		Graphics.set_text_size offset;
		Graphics.set_color (Graphics.rgb 100 200 100);
		foreach_list (get_intro (!time/ speed)) 
			(fun line -> begin
				Graphics.draw_string line;
				Graphics.rmoveto offset (-offset)
			end)
	end;;

"Init";;
	let init_camera size_x size_y ouverture =
		(*initialise la camera virtuelle*)
		let i = new vector3D 1. 0. 0. and
			j = new vector3D 0. 1. 0. and
			k = new vector3D 0. 0. 1. and
			p = new vector3D 0. 0. 0. in
		let repereCamera = {e1 = i; e2 = j; e3 = k; o = p} and
			camera = create_camera size_x size_y ouverture in
		repereCamera, camera;;

	let run_start () = 
	(*lance le jeu!!*)
	(*initialise toutes les classe requises au lancement du jeu*)
		let repereCamera, camera = init_camera 1020 620 1.1 in
		let game = new game camera repereCamera and
			intro = new intro and
			playIntro = ref true and
			run = ref true in
		create_windows camera;
		let timeFps = ref (Unix.gettimeofday ()+.1.) and 
			timeUpdate = ref (Unix.gettimeofday ()+.1.) in
		(*Pour avoir des statistiques sur les performances de la simulation*)
		(*les grandeurs sont doubler afin d'ajoir la performance instantané et moyenne*)
		let startTime = !timeFps and 
			frames = ref 0 and 
			ticks = ref 0 and 
			emptyLoops = ref 0 and
			frame = ref 0 and
			tick = ref 0 and
			emptyLoop = ref 0 and
			timeDisplay = ref (Unix.gettimeofday ()) in
		try
			while !run do
				if Unix.gettimeofday () -. !timeDisplay > 1. then begin
				(*on affiche les performance toutes les secondes, indépendament de l'avancement des calcul du jeu ou de rendus*)
					display_screen tick frame emptyLoop;
					timeDisplay := !timeDisplay +. 1.
				end;
				if (Unix.gettimeofday ()) -. !timeUpdate >= frecUpdate then begin(*on fait un tour d'update du jeu*)
					(*s'il c'est écouler sufisament de temps entre les derniers calculs de jeu et l'instant actuelle, on refait des calcul de jeu*)
					timeUpdate := !timeUpdate +. frecUpdate;
					if !playIntro then begin
						intro#update ();
						playIntro := not (Graphics.button_down ())
					end
					else run := not (game#update ());
					incr ticks;
					incr tick;
				end
				else begin
					if (Unix.gettimeofday ()) -. !timeFps >= frecFps then begin(*on fait un tour de rendu du jeu*)
						(*s'il ne c'est pas écouler sufisament de temps entre deux calcul de jeu succéssif, on regarde si le temps écouler entre deux calculs d'affichage lui est sufisament long*)
						timeFps := !timeFps +. frecFps;
						clear_windows camera;
						if !playIntro then intro#render camera
						else game#render ();
						incr frames;
						incr frame;
					end
					else begin
						(*si le temmps écouler etre les render et les update n'est pas asser long, on ne fait rien. En pratque cela 'arrive que l'orsque le joueur est dans le menu*)
						incr emptyLoops;
						incr emptyLoop
					end
				end
			done;
			stats !ticks !frames !emptyLoops startTime;
			Graphics.close_graph ()
		with |Graphics.Graphic_failure "graphic screen not opened" -> stats !ticks !frames !emptyLoops startTime;;	

(*On lance le jeu!*)
run_start ();;