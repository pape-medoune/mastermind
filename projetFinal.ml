(* Définition des types *)
type couleur = Rouge | Vert | Jaune | Bleu | Magenta | Blanc | Noir | Cyan
type sequence = couleur list

(* Fonction pour générer une séquence aléatoire *)
let generer_sequence n couleurs =
 let rec aux n acc =
    if n = 0 then acc
    else
      let couleur_aleatoire = List.nth couleurs (Random.int (List.length couleurs)) in
      aux (n-1) (couleur_aleatoire :: acc)
 in aux n []

(* Fonction pour analyser deux séquences et compter les correspondances *)
let analyser_sequences sequence_cachee sequence_proposee =
 let rec aux sc sp bien_place mal_place =
    match sc, sp with
    | [], _ | _, [] -> (bien_place, mal_place)
    | h1::t1, h2::t2 ->
        if h1 = h2 then aux t1 t2 (bien_place + 1) mal_place
        else if List.mem h2 sc then aux t1 t2 bien_place (mal_place + 1)
        else aux t1 t2 bien_place mal_place
 in aux sequence_cachee sequence_proposee 0 0

(* Fonction pour lire une séquence de l'utilisateur *)
let lire_sequence () =
 read_line () |> String.split_on_char ' ' |> List.map (fun s -> match s with
    | "Rouge" -> Rouge
    | "Vert" -> Vert
    | "Jaune" -> Jaune
    | "Bleu" -> Bleu
    | "Magenta" -> Magenta
    | "Blanc" -> Blanc
    | "Noir" -> Noir
    | "Cyan" -> Cyan
    | _ -> failwith "Couleur non reconnue")

(* Fonction pour demander le nom du joueur *)
let demander_nom_joueur () =
 print_endline "Entrez votre nom :";
 read_line ()

(* Fonction principale pour jouer le jeu *)
let jouer_mastermind () =
 let couleurs = [Rouge; Vert; Jaune; Bleu; Magenta; Blanc; Noir; Cyan] in
 let sequence_cachee = generer_sequence 4 couleurs in
 let essais_max = 10 in (* Définir ici le nombre maximal d'essais *)
 let nom_joueur = demander_nom_joueur () in
 print_endline ("Bienvenue, " ^ nom_joueur ^ " ! Voici les couleurs disponibles : Rouge, Vert, Jaune, Bleu, Magenta, Blanc, Noir, Cyan");
 let rec jouer_tour essai =
    if essai > essais_max then
      begin
        print_endline "Vous avez dépassé le nombre d'essais autorisé. Vous avez perdu !";
        exit 0 (* Terminer le programme *)
      end;
    Printf.printf "Essai %d/%d. Proposez une séquence : " essai essais_max;
    let sequence_proposee = lire_sequence () in
    let (bien_place, mal_place) = analyser_sequences sequence_cachee sequence_proposee in
    if bien_place = List.length sequence_cachee then begin
      print_endline "Bravo, vous avez trouvé la séquence !";
      exit 0 (* Terminer le programme en cas de victoire *)
    end else begin
      Printf.printf "Bien placés : %d, Mal placés : %d\n" bien_place mal_place;
      jouer_tour (essai + 1) 
    end
 in jouer_tour 1 (* Commencer avec l'essai 1 *)

(* Lancement du jeu *)
let () = Random.self_init (); jouer_mastermind ()
