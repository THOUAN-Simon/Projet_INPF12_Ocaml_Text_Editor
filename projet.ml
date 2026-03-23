let valid_chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
;;

(* zipper dont les éléments sont de type 'a et l'élément courrant de type 'b *)

type ('a,'b) zipper =
  { before:  'a list (* liste des éléments précédents dans l'ordre inversé *)
    ; current: 'b
    ; after:   'a list (* liste des éléments précédents dans l'ordre *)
    ; pos:     int (* position courrante *)
    }
  ;;

type cursor = Cursor;;

type line   = (char, cursor) zipper;;

type buffer = (line, line) zipper;;

type action =
    | Up
    | Left
    | Down
    | Right
    | Char of char
    | Newline
    | Delete
    | Backspace
;;

let empty_line = { before = []; current = Cursor; after = []; pos = 0 };;

let empty_buf  = { before = []; current = empty_line; after = []; pos = 0 };;

(** NE RIEN MODIFIER AVANT CE POINT **)

let sx = 800 (* LARGEUR DE LA FENETRE GRAPHIQUE EN POINTS *)

let sy = 600  (* HAUTEUR DE LA FENETRE GRAPHIQUE EN POINTS *)

(*fonction auxilliaire utilisée pour connaître la position la plus à droite du curseur (sert à connaître le nombre de caractère d'une ligne) *)
let rec last_cur_pos l = 
  match (l.before,l.after) with
  |([],[]) -> 0
  |([], t :: q) -> 1 + last_cur_pos {before = l.before; current = l.current; after = q; pos = l.pos}
  |(t :: q, []) -> 1 + last_cur_pos {before = q; current = l.current; after = l.after; pos = l.pos}
  |(tb :: qb, ta :: qa) -> 2 + last_cur_pos {before = qb; current = l.current; after = qa; pos = l.pos}

(* fonction auxilliaire utilie pour l'amélioration de move_up et move_down *)
let rec replace_cursor l new_pos =
  if (new_pos > l.pos) then
    match (l.before, l.after) with
    |([],[]) -> l
    |(tb :: qb, []) -> l
    |(_, ta :: qa) -> let replaced_cur_1_step = {before = ta :: l.before; current = l.current ; after = qa; pos = l.pos + 1} 
                      in replace_cursor replaced_cur_1_step new_pos
  else if (new_pos < l.pos) then
    match (l.before, l.after) with
    |([],[]) -> l
    |([], ta :: qa) -> l
    |(tb :: qb, _) -> let replaced_cur_1_step = {before = qb; current = l.current ; after = tb :: l.after; pos = l.pos - 1} 
                      in replace_cursor replaced_cur_1_step new_pos
  else l


(** [get_current z] renvoie la valeur courante du zipper z
    @param: z un zipper
    @return: la valeur courante du zipper z
    @type: ('a, 'b) zipper -> 'b = <fun>
    @requires: z est un ('a, 'b) zipper
    @ensures: get_current renvoie la valeur du champ current de z
    @raises: rien
*)
let get_current z = z.current
      

(** [get_pos z] renvoie la position courante du zipper z
    @param: z un zipper
    @return: la position courante du zipper z
    @type: ('a, 'b) zipper -> int = <fun>
    @requires: z est un ('a, 'b) zipper
    @ensures: get_pos renvoie la valeur du champ pos du zipper z
    @raises: rien
*)
let get_pos z = z.pos


(** [fold f g z acc0] fold sur le type des zipper
    @param: f est une fonction, g une fonction, acc0 un constante et z un zipper
    @return: le résultat d'un fold sur z
    @type: (’a -> ’c -> ’c) -> (’b -> ’c -> ’d) -> (’a,’b) zipper -> ’c -> ’d = <fun>
    @requires: z est un (’a,’b) zipper, f est  de type (’a -> ’c -> ’c), g de type (’b -> ’c -> ’d) et acc0 de type 'c. 
    En particulier, si g est de type (’b -> ’c -> ’c) alors 
    fold est de type : (’a -> ’c -> ’c) -> (’b -> ’c -> ’c) -> (’a,’b) zipper -> ’c -> ’c = <fun>.
    @ensures: Applique fold_right à z.before et z.left en utilisant f puis utilise g pour renvoyer un résultat de type 'd 
    en utilisant la valeur de z.current.
    @raises: rien
*)
let fold f g z acc0 =
  let acc_temp = List.fold_right f z.before acc0 in
  let acc_temp2 = List.fold_right f z.after acc_temp in
  g z.current acc_temp2


(** [update_with f z] opération sur le champ current de z
    @param: f est une fonction, z un zipper
    @return: application de f sur z.current
    @type: (’b -> ’c) -> (’a,’b) zipper -> (’a,’c) zipper = <fun>
    @requires: z est un (’a,’b) zipper, f est  de type 'b -> ’c
    @ensures: Applique f au champ current de b, et retourn un nouveau zipper dont le champ current vaut f (z.current)
    @raises: rien
*)
let update_with f z =
  {before = z.before; current = (f z.current); after = z.after; pos = z.pos}


(** [move_left buf] déplace le curseur d'un pas vers la gauche
    @param: buf de type "buffer"
    @return: un buffer
    @type: buffer -> buffer = <fun>
    @requires: buf est de type buffer
    @ensures: le buffer retourné correspond à avoir déplacé le curseur de la ligne courante du buffer passé en argument d'un pas sur la gauche,
    toute chose égale par ailleurs (aucune modification dans le contenu des lignes, seul le curseur se déplace à l'écran).
    @update: si on se trouve tout au bout à gauche, renvoie le curseur au début de la ligne précédente.
    @raises: rien
*)
let move_left buf = 
  match ((buf.current).before) with
  |[] ->(match buf.before with
         |[] -> buf
         |tb :: qb -> let new_cursor_pos = {before = (List.rev tb.after) @ tb.before; current = tb.current; after = []; pos = last_cur_pos tb} 
                      in {before = qb; current = new_cursor_pos; after = buf.current :: buf.after; pos = buf.pos-1})
  |tl :: ql -> let new_cur_line = {before = ql; current = (buf.current).current; after = tl :: (buf.current).after; pos = (buf.current).pos-1} 
               in {before = buf.before; current = new_cur_line; after = buf.after; pos = buf.pos}

(** [create_newline buf] ajoute une nouvelle ligne vide
    @param: buf est un buffer
    @return: un buffer
    @type: buffer -> buffer = <fun>
    @requires: buf de type buffer
    @ensures: le buffer retourné contient à la ligne courante une nouvelle ligne vide
    @update: si la création de la nouvelle ligne s'est fait au milieu d'une ligne non vide, le contenu après le curseur
    est transféré sur la nouvelle ligne, et le contenu précédant le curseur est laissé sur la ligne courante du buffer passé en argument
    @raises: rien
*)
let create_newline buf = 
  match ((buf.current).after) with 
  |[] -> {before = buf.current :: buf.before; current = empty_line; after = buf.after; pos = buf.pos+1}
  |l -> let muted_buf_current = {before = (buf.current).before; current = (buf.current).current; after = []; pos = (buf.current).pos} 
     in let muted_newline = {before = []; current = (buf.current).current; after = l; pos = 0} 
     in {before = muted_buf_current :: buf.before; current = muted_newline; after = buf.after; pos = buf.pos+1}

(** [move_right buf] déplace le curseur d'un pas vers la droite
    @param: buf de type "buffer"
    @return: un buffer
    @type: buffer -> buffer = <fun>
    @requires: buf est de type buffer
    @ensures: le buffer retourné correspond à avoir déplacé le curseur de la ligne courante du buffer passé en argument d'un pas sur la droite,
    toute chose égale par ailleurs.
    @update: si on se trouve tout au bout à droite, renvoie le curseur au début de la ligne suivante.
    @raises: rien
*)
let move_right buf = 
  match ((buf.current).after) with
  |[] ->(match buf.after with
         |[] -> create_newline buf
         |tb :: qb -> let new_cursor_pos = {before = []; current = tb.current; after = (List.rev tb.before) @ tb.after; pos = 0} 
                      in {before = buf.current :: buf.before; current = new_cursor_pos; after = qb; pos = buf.pos+1})
  |tl :: ql -> let new_cur_line = {before = tl :: (buf.current).before; current = (buf.current).current; after = ql; pos = (buf.current).pos+1} 
               in {before = buf.before; current = new_cur_line; after = buf.after; pos = buf.pos}


(** [move_up buf] déplace le curseur à la ligne précédente
    @param: buf de type "buffer"
    @return: un buffer
    @type: buffer -> buffer = <fun>
    @requires: buf est de type buffer
    @ensures: le buffer retourné correspond à avoir déplacé le curseur de la ligne courante du buffer passé en argument vers la ligne précédente, 
    toute chose égale par ailleurs.
    @update: si on se trouve tout en haut (donc sur la première ligne), le curseur revient au début de la ligne. Le mouvement vers la ligne précédente ne
    fait plus atterir le curseur à la fin de la ligne précédente, mais à la même position que la ligne courante.
    @raises: rien
*)
let move_up buf = 
  match buf.before with 
  |[] -> (match (buf.current).before with 
              |[] -> buf
              |l -> let new_cursor_pos = {before = []; current = (buf.current).current; after = (List.rev (buf.current).before) @ (buf.current).after; pos = 0} 
                    in {before = []; current = new_cursor_pos; after = buf.after; pos = buf.pos})
  |t :: q -> let new_cursor_pos = replace_cursor t ((buf.current).pos)
             in {before = q; current = new_cursor_pos; after = buf.current :: buf.after; pos = buf.pos-1}


(** [move_down buf] déplace le curseur à la ligne précédente
    @param: buf de type "buffer"
    @return: un buffer
    @type: buffer -> buffer = <fun>
    @requires: buf est de type buffer
    @ensures: le buffer retourné correspond à avoir déplacé le curseur de la ligne courante du buffer passé en argument vers la ligne suivante,
    toute chose égale par ailleurs.
    @update: Le mouvement vers la ligne suivante ne fait plus atterir le curseur à la fin de la ligne suivante, mais à la même position que la ligne courante.
    @raises: rien
    @raises: rien
*)
let move_down buf = 
  match buf.after with 
  |[] -> (match (buf.current).after with 
              |[] -> buf
              |l -> let new_cursor_pos = {before = (List.rev (buf.current).after) @ (buf.current).before; current = (buf.current).current; after = []; pos = last_cur_pos buf.current} 
                    in {before = buf.before; current = new_cursor_pos; after = buf.after; pos = buf.pos})
  |t :: q -> let new_cursor_pos = replace_cursor t ((buf.current).pos)
             in {before = buf.current :: buf.before ; current = new_cursor_pos; after = q; pos = buf.pos+1}

(** [insert_char ch buf] ajoute le caractère ch à la position courante
    @param: ch est un caractère, buf est un buffer
    @return: un buffer
    @type: char -> buffer -> buffer = <fun>
    @requires: ch de type char, buf de type buffer
    @ensures: le buffer retourné contient un caractère supplémentaire (qui est ch) à la ligne courante. L'insertion se fait à gauche du curseur,
    toute choses égales par ailleurs (les autres lignes ne sont pas modifiées).
    @raises: rien
*)
let insert_char ch buf = update_with (fun z -> {before = ch :: z.before; current = z.current; after = z.after; pos = z.pos+1}) buf


(** [do_suppr buf] supprime à la position courante
    @param: buf est un buffer
    @return: un buffer
    @type: buffer -> buffer = <fun>
    @requires: buf de type buffer
    @ensures: le buffer retourné contient un caractère en moins (qui est le caractère courant) à la ligne courante. La suppression se fait à droite du curseur,
    toute choses égales par ailleurs.
    @update: si le curseur se trouve en fin de ligne, la suppression se poursuit sur la ligne courante, en supprimant la ligne suivante 
    (son contenu est ramené à droite du curseur pour continuer la suppression).
    @raises: rien
*)
let do_suppr buf = 
  match (buf.current).after with
  |[] -> (match buf.after with
          |[] -> buf
          |t :: q -> let new_current_line = {before = (buf.current).before; current = (buf.current).current; after = List.rev ((List.rev t.after) @ t.before); pos = (buf.current).pos} 
                     in {before = buf.before; current = new_current_line; after = q; pos = buf.pos})
  |t :: q -> let new_current_line = {before = (buf.current).before; current = (buf.current).current; after = q; pos = (buf.current).pos} 
             in {before = buf.before; current = new_current_line; after = buf.after; pos = buf.pos} 


(** [do_backspace buf] supprime le caractère avant le curseur (avant la position courante)
    @param: buf est un buffer
    @return: un buffer
    @type: buffer -> buffer = <fun>
    @requires: buf de type buffer
    @ensures: le buffer retourné contient un caractère en moins (qui est le caractère précédant le caractère courant) à la ligne courante. 
    La suppression se fait à gauche du curseur, toute choses égales par ailleurs.
    @update: si le curseur se trouve en début de ligne, la suppression se poursuit sur la ligne précédente, en ramenant la ligne courante avec le curseur
    @raises: rien
*)
let do_backspace buf = 
  match (buf.current).before with
  |[] -> (match buf.before with
          |[] -> buf
          |t :: q -> let new_current_line = {before = (List.rev t.after) @ t.before; current = (buf.current).current; after = (buf.current).after; pos = last_cur_pos t} 
                     in {before = q; current = new_current_line; after = buf.after; pos = buf.pos-1})
  |t :: q -> let new_current_line = {before = q; current = (buf.current).current; after = (buf.current).after; pos = (buf.current).pos} 
             in {before = buf.before; current = new_current_line; after = buf.after; pos = buf.pos} 
(***** NE RIEN MODIFIER À PARTIR DE CE POINT **)       

let apply_action a buf =
    match a with
    | Up        -> move_up    buf
    | Left      -> move_left  buf
    | Down      -> move_down  buf
    | Right     -> move_right buf
    | Char ch   -> insert_char ch buf
    | Newline   -> create_newline buf
    | Delete    -> do_suppr buf
    | Backspace -> do_backspace buf
;;
let wopen () =
  let args = Printf.sprintf " %dx%d" sx sy in
  let _ = Graphics.open_graph args in
  let _ = Graphics.set_window_title "test" in
  ()

let font_width,font_height = 18,18

let line_height = font_height + 4

let line_width = font_width + 4
             
let default_char = Char.chr 167 



                 
let draw_square col row color c =
  let _ =
    Graphics.moveto (col*line_width+4) (Graphics.size_y () - row * line_height +2) in
  let _ = Graphics.set_color color in
  let _ = Graphics.fill_rect (col*(line_width)) (Graphics.size_y () - row * (line_width)) (line_width) (line_height) in
  let _ = Graphics.set_color Graphics.black in
  let _ = Graphics.draw_rect (col*(line_width)) (Graphics.size_y () - row * (line_width)) (line_width) (line_height)
  in
  Graphics.draw_char c



let draw_line is_current row l =
  let print i c =
    let _ = draw_square i row Graphics.white c in
    i+1
  in
  let col = List.fold_right (fun c i -> print i c) l.before 0 in
  let _ = List.fold_left print col l.after in 
  let _ =
    if is_current
    then
    let _ = Graphics.set_color Graphics.red in
      let _ = Graphics.fill_rect (col*(line_width)-2) (Graphics.size_y () - row * (line_width)) (4) (line_height) in
      Graphics.set_color Graphics.black
    else ()
  in
  ()

let draw_buffer buf =
  let print b j l =
    let _ = Format.printf "line : %d@." j in
    let _ = draw_line b j l in
    j+1
  in
  let row = List.fold_right (fun l j -> print false j l) buf.before 1 in
  let _ = print true row buf.current in
  List.fold_left (print false) (row+1) buf.after
  
  
let rec loop  buf =
  let _ = Graphics.clear_graph () in 
  let _ = draw_buffer buf in 
  let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
  let ch = ev.Graphics.key in
  if Char.code ch = 27 (* esc *) 
  then ()
  else 
    let laction = [
        Char.chr 26,Up;
        Char.chr 19,Down;
        Char.chr 17,Left;
        Char.chr 4,Right;
        Char.chr 13,Newline;
        Char.chr 127,Delete;
        Char.chr 8,Backspace
      ]
    in
    let buf1 = 
      match List.assoc_opt ch laction with
      | Some a -> apply_action a buf
      | None ->
                  if String.contains valid_chars ch
         then apply_action  (Char ch) buf
         else
           let code = Char.code ch in
           let msg = if code >= 1 && code <= 26
                     then Format.sprintf " (CTRL + %c)" (Char.chr (Char.code 'A' + code -1 ))
                     else ""
           in
           let _ = 
             Format.fprintf Format.err_formatter
               "Invalid char : ascii code %d %s@."
               code
               msg
           in 
           buf
    in
    loop buf1
  
let main () =
  let _ = wopen () in
  let _ = loop empty_buf in 
  let _ = Graphics.close_graph () in
  ()

let _ = main  ()

(** -- Tests --
let z1 = {before = []; current = Cursor; after = ['a';'b';'c';'d';'e']; pos = 0};;
let z2 = {before = ['h'; 'g'; 'f']; current = Cursor; after = ['i'; 'j']; pos = 3};;
let z3 = {before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5};;
let buff0 = {before = []; current = z1; after = [z2; z3]; pos = 0};;
let buff1 = {before = [z1]; current = z2; after = [z3]; pos = 1};;
let buff2 = {before = [z1; z2]; current = z3; after = []; pos = 2};;
let buff3 = {before = []; current = z3; after = [z2; z1]; pos = 0};;
let buff4 = {before = [z2]; current = z1; after = [z3]; pos = 1};;
let f a c = a = 'h' || c;;
let g b c = b = Cursor || c;;
let h x = empty_line;;


let () =
  assert (last_cur_pos z1 = 5);
  assert (replace_cursor z2 0 = {before = []; current = Cursor; after = ['f'; 'g'; 'h'; 'i'; 'j']; pos = 0});
  assert (get_current z1 = Cursor);
  assert (get_pos z3 = 5);

  (*test avec des fonctions respectant le type de l'énoncé*)
  assert (fold f g z2 true = true);

  (*remplace la ligne courante de buff par une ligne vide*)
  assert (update_with h buff1 = {before = [z1]; current = empty_line; after = [z3]; pos = 1});

  (*move_left classique*)
  assert (move_left buff1 = {before =
  [{before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0}];
  current =
  {before = ['g'; 'f']; current = Cursor; after = ['h'; 'i'; 'j']; pos = 2};
  after =
  [{before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5}];
  pos = 1});

  (*move_left qui ne doit rien faire (on est tout en haut du fichier au début de la ligne*)
  assert (move_left buff0 = buff0);

 (*illustre le déplacement des caractères i et j sur une nouvelle ligne *)
 assert (create_newline buff1 = {before =
 [{before = ['h'; 'g'; 'f']; current = Cursor; after = []; pos = 3};
  {before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0}];
  current = {before = []; current = Cursor; after = ['i'; 'j']; pos = 0};
  after =
 [{before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5}];
  pos = 2});

 (*move_right classique*)
 assert (move_right buff1 = {before =
  [{before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0}];
 current =
  {before = ['i'; 'h'; 'g'; 'f']; current = Cursor; after = ['j']; pos = 4};
 after =
  [{before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5}];
 pos = 1});

 (*move_right qui doit créer une nouvelle ligne vide (car à la fin du document) *)
 assert (move_right buff2 = {before =
 [{before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5};
  {before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0};
  {before = ['h'; 'g'; 'f']; current = Cursor; after = ['i'; 'j']; pos = 3}];
  current = {before = []; current = Cursor; after = []; pos = 0}; after = [];
  pos = 3});

  (*move_up classique*)
 assert (move_up buff1 = {before = [];
 current =
  {before = ['c'; 'b'; 'a']; current = Cursor; after = ['d'; 'e']; pos = 3};
 after =
  [{before = ['h'; 'g'; 'f']; current = Cursor; after = ['i'; 'j']; pos = 3};
   {before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5}];
 pos = 0});

 (*move_up en haut du fichier, il doit donc décaler le curseur au début de la ligne courante*)
 assert (move_up buff0 = {before = [];
 current =
  {before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0};
 after =
  [{before = ['h'; 'g'; 'f']; current = Cursor; after = ['i'; 'j']; pos = 3};
   {before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5}];
 pos = 0});

 (*move_down classique*)
 assert (move_down buff1 = {before =
 [{before = ['h'; 'g'; 'f']; current = Cursor; after = ['i'; 'j']; pos = 3};
  {before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0}];
 current =
 {before = ['m'; 'l'; 'k']; current = Cursor; after = ['n'; 'o']; pos = 3};
 after = []; pos = 2});

 (*move_down en bas du fichier et à la fin de la ligne courante, il doit donc ne rien faire car il y est déjà*)
 assert (move_down buff2 = buff2);

 (*insert_char classique*)
 assert (insert_char 'z' buff1 = {before =
 [{before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0}];
 current =
 {before = ['z'; 'h'; 'g'; 'f']; current = Cursor; after = ['i'; 'j'];
  pos = 4};
 after =
 [{before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5}];
 pos = 1});

 (*do_suppr classique*)
 assert (do_suppr buff1 = {before =
 [{before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0}];
 current = {before = ['h'; 'g'; 'f']; current = Cursor; after = ['j']; pos = 3};
 after =
 [{before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5}];
 pos = 1});

 (*do_suppr à la fin d'une ligne*)
 assert (do_suppr buff3 = {before = [];
 current =
  {before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor;
   after = ['f'; 'g'; 'h'; 'i'; 'j']; pos = 5};
 after =
  [{before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0}];
 pos = 0});

  (*do_backspace classique*)
  assert (do_backspace buff1 = {before =
  [{before = []; current = Cursor; after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 0}];
 current = {before = ['g'; 'f']; current = Cursor; after = ['i'; 'j']; pos = 3};
 after =
  [{before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5}];
 pos = 1});

 (*do_backspace au début d'une ligne*)
 assert (do_backspace buff4 = {before = [];
 current =
  {before = ['j'; 'i'; 'h'; 'g'; 'f']; current = Cursor;
   after = ['a'; 'b'; 'c'; 'd'; 'e']; pos = 5};
 after =
  [{before = ['o'; 'n'; 'm'; 'l'; 'k']; current = Cursor; after = []; pos = 5}];
 pos = 0});
 **)