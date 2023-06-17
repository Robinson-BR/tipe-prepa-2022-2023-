(* truc trouvé sur internet que je garde *)

(*
  let () =
    (* charge l'image donnée en argument : "./test truc.png" *)
    let test = load_rgb_matrix Sys.argv.(1) in
    Graphics.open_graph " 800x600";
    (* dessine l'image une première fois *)
    Graphics.draw_image (to_graphics test) 0 0;
    ignore (Graphics.read_key ());
    (* dessine l'image avec les couleurs inversées *)
    Graphics.draw_image (to_graphics (invert_colors test)) 0 0;
    ignore (Graphics.read_key ());
    Graphics.close_graph ()
*)

(*Début du code*)
(*------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* PARTIE QUI CONTIENT LES FONCTIONS DE CHARGEMENT DES IMAGES À PARTIR DE FICHIERS ET QUI LES PASSES DE RGB À NUANCES DE GRIS*)

(* charge une image quelconque (.png ... comme supporté par
camlimages) vers une matrice de triplets (r,g,b) d'entiers :
(int*int*int)*array*array *)
let load_rgb_matrix name =
  let img = Images.load name [] in
  let gimg = Graphic_image.array_of_image img in
  let rgb color =
    let quot n = n mod 256, n / 256 in
    let b, rg = quot color in
    let g, r = quot rg in
    r, g, b
  in
  Array.map (Array.map rgb) gimg


  (* Cette fonction sert à passer d'une matrice en couleur RGB de la forme array array (int*int*int)
à une matrice de nuances de gris *)
let rgb_matrix_to_gray_matrix matrix = 
  let rgb_to_gray (r,g,b) = (* Cette sous-fonction sert convertir un pixel RGB en un pixel en nuances de gris *)
    let x = int_of_float (0.299*.(float_of_int r) +. 0.5870*.(float_of_int g) +. 0.114*.(float_of_int b)) in
    x
  in
  let map_matrix f matrix = (* Cette sous-fonction sert à appliquer une fonction à tout les éléments d'une matrice *)
    Array.map (Array.map f) matrix
  in
  map_matrix rgb_to_gray matrix




(*------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* PARTIE QUI CONTIENT LES FONCTIONS NÉCESSAIRES À L'AFFICHAGE DES IMAGES *)

(* transforme une matrice de triplets (r,g,b) en une "image graphics" de type Graphics.image *)
let to_graphics rgb_matrix =
  Graphics.make_image
    (Array.map
        (Array.map
          (fun (r, g, b) -> Graphics.rgb r g b))
        rgb_matrix)


(*permet l'affichage d'une matrice RGB représentant une image (dans le contexte d'un open_graph)*)
let print_matrix matrix =
  Graphics.draw_image (to_graphics matrix) 0 0;
  ignore (Graphics.read_key ())


(* transforme une image en nuances de gris de la forme Array Array int en Array Array (int * int * int) *)
(* elle permet de rendre l'image en nuance de gris affichable *)
let adapt_gray_matrix matrix =
  let n = Array.length matrix in
  let p = Array.length matrix.(0) in 
  let out = Array.make_matrix n p (0,0,0) in
  for i=0 to n-1 do 
    for j=0 to p-1 do 
      let a = matrix.(i).(j) in 
      out.(i).(j) <- (a,a,a);
    done;
  done;
  out


(*permet l'affichage d'une matrice en nuances de gris représentant une image (dans le contexte d'un open_graph)*)
let print_gray_matrix matrix =
  let temp = adapt_gray_matrix matrix in
  Graphics.draw_image (to_graphics temp) 0 0;
  ignore (Graphics.read_key ())



(*---------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* PARTIE SUR LES FILTRES*)

(*la matrice d'un filtre de contour*)
let contour = [|[|-1;-1;-1|]; [|-1;8;-1|];[|-1;-1;-1|]|]

(*
let flou = [|[|1;2;1|]; [|2;4;2|];[|1;2;1|]|]
*)

(* applique un filtre par convolution de noyeau de dimension 3x3 à une image en nuances de gris*)
let filter_gray_3x3  filtre facteur matrix =
  let n = Array.length matrix in 
  let p = Array.length matrix.(0) in 
  let extended_matrix = Array.make_matrix (n+2) (p+2) 0 in

  for i=0 to n-1 do
    for j=0 to p-1 do 
      extended_matrix.(i+1).(j+1) <- matrix.(i).(j);
    done;
  done;

  for i=0 to n-1 do
    extended_matrix.(i+1).(0) <- matrix.(i).(0);
    extended_matrix.(i+1).(p+1) <- matrix.(i).(p-1);
  done;

  for j=0 to p+1 do 
    extended_matrix.(0).(j) <- extended_matrix.(1).(j);
    extended_matrix.(n+1).(j) <- extended_matrix.(n).(j);
  done;

  let out = Array.make_matrix n p 0 in
  
  for i=0 to n-1 do 
    for j=0 to p-1 do 
      let acc = ref 0 in 
      for k=0 to 2 do 
        for l=0 to 2 do 
          acc := !acc + filtre.(k).(l) * extended_matrix.(i+k).(j+l);
        done;
      done;
      out.(i).(j) <- (!acc / facteur);
      if out.(i).(j) > 255 then
        out.(i).(j) <- 255;
      if out.(i).(j) < 0 then
        out.(i).(j) <- 0;
    done;
  done;
  out



(*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*PARTIE SUR LE SEUILLAGE (AVEC LA MÉTHODE D'OTSU)*)

(* Calcule l'histograme des valeurs d'une matrice en nuances de gris*)
let histogramme matrix =
  let n = Array.length matrix in 
  let p = Array.length matrix.(0) in 
  let out = Array.make 256 0 in
  for i=0 to n-1 do 
    for j=0 to p-1 do 
      out.(matrix.(i).(j)) <- out.(matrix.(i).(j)) + 1;
    done;
  done;
  out


  (* Calcule les probabilitées de chaque colonne de l'histogramme *)
let probabilite matrix =
  let out = Array.make 256 0. in 
  let hist = histogramme matrix in
  let n = Array.length matrix in 
  let p = Array.length matrix.(0) in 
  let total = n*p in 

  for i=0 to 255 do 
    out.(i) <- (float_of_int hist.(i)) /. (float_of_int total);
  done;
  out


(*calcule le seuil optimal avec la méthode d'Otsu*)
let otsu matrix = 
  let proba = probabilite matrix in 

  let variance_interclasse = ref 0. in 
  let plus_grande_variance_interclasse = ref 0. in 
  let seuillage = ref 0 in 

  let w0 = ref 0.00000000000000000000000000000001 in 
  let w1 = ref 1. in 

  let mu0 = ref 0. in
  let mu1 = ref 0. in
  for i=0 to 255 do
    mu1 := !mu1 +. ((float_of_int i) *. proba.(i));
  done;
  mu1 := !mu1 /. !w1;


  for k=1 to 255 do 
    mu0 := !mu0 *. !w0;
    mu1 := !mu1 *. !w1;

    w0 := !w0 +. proba.(k-1);
    w1 := !w1 -. proba.(k-1);

    mu0 := (!mu0 +. (float_of_int (k-1)) *. proba.(k-1)) /. !w0;
    mu1 := (!mu1 -. (float_of_int (k-1)) *. proba.(k-1)) /. !w1;

    variance_interclasse := (!w0 *. !w1) *. ((!mu0 -. !mu1) ** 2.);

    if variance_interclasse > plus_grande_variance_interclasse then
      (plus_grande_variance_interclasse := !variance_interclasse;
      seuillage := k;)

  done;
  !seuillage

(*applique le seuillage à la matrice*) 
  let seuillage matrix =
    let n = Array.length matrix in 
    let p = Array.length matrix.(0) in 
    let out = Array.make_matrix n p 0 in 
    let seuil = otsu matrix in 
    
    for i=0 to n-1 do 
      for j=0 to p-1 do 
          if matrix.(i).(j) < seuil then
            out.(i).(j) <- 0
          else
            out.(i).(j) <- 255;
      done;
    done;
    out
  

(*
(*-----------------------------------------------------------------------------------------------------------------------------------*)
(* PARTIE SUR L'ALGORITHME DES K-MOYENNES*)

(*calcule la distance entre deux matrices binarisée*)
let distance matrix1 matrix2 =
  let out = ref 0 in
  let n = Array.length matrix1 in
  let p = Array.length matrix1.(0) in
  for i=0 to n-1 do
    for j=0 to p-1 do
      if matrix1.(i).(j) <> matrix2.(i).(j) then
        out := !out + 1;
    done;
  done;
  !out


(*calcule la distance moyenne d'une matrice binarisée à un groupe de matrice binarisées*)
let distance_groupe matrix groupe =
  let l = Array.length groupe in 
  let out = ref 0 in 
  for i=0 to l-1 do 
    out := !out + (distance matrix groupe.(i));
  done;
  out := !out / l;
  !out 


(*détermine le groupe d'images dans groupes dont matrix est la plus proche*)
let attribution_groupe matrix groupes =
  let l = Array.length groupes in 
  let out = ref 0 in 
  let temp = ref 0 in
  for i=0 to l-1 do 
    let x = distance_groupe matrix groupes.(i) in
    if x < !temp then 
      (temp := x;
      out := i;)
  done;
  !out

*)

(*--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* PARTIE EXECUTIVE DU PROGRAMME *)
let () =
  (* charge l'image donnée en argument : "./main.exe truc.png" *)
  let mat = load_rgb_matrix Sys.argv.(1) in
  let gray_mat = ref (rgb_matrix_to_gray_matrix mat) in


  Graphics.open_graph "";
  print_matrix mat;
  print_gray_matrix !gray_mat;

(*
  gray_mat := filter_gray_3x3 flou 9 !gray_mat;
  print_gray_matrix !gray_mat;
*)

  gray_mat := seuillage !gray_mat;
  print_gray_matrix !gray_mat;

  gray_mat := filter_gray_3x3 contour 1 !gray_mat;
  print_gray_matrix !gray_mat;
  Graphics.close_graph ();



