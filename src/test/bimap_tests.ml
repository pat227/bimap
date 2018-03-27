module Bimap = Bimap.Bimap
module Bimap_multi = Bimap_multi.Bimap_multi
open OUnit2
module Bimap_tests = struct

  let oc = Core.Out_channel.stdout;;    
  let print_n_flush s =
    Core.Out_channel.output_string oc s;
    Core.Out_channel.flush oc;;
  
  let rec print_n_flush_alist ~sep ~to_string_func l =
    match l with
    | [] -> ()
    | h :: t ->
       let s = to_string_func h in 
       let () = print_n_flush (s ^ sep) in
       print_n_flush_alist ~sep ~to_string_func t;;

  let rec print_n_flush_alistlist ~sep ~to_string_func l =
    match l with
    | [] -> ()
    | h :: t ->
       let () = print_n_flush " <|> " in 
       let () = print_n_flush_alist ~sep ~to_string_func h in
       print_n_flush_alistlist ~sep ~to_string_func t;;
(*
  let rec list_list_mem l v =
    match l with
    | [] -> false
    | h::t ->
       if Core.List.mem ~equal:(fun x y -> Core.String.equal x y) h v then true else
	 list_list_mem t v

  let rec key_list_list_mem l v =
    match l with
    | [] -> false
    | h::t ->
       if Core.List.mem ~equal:(fun x y -> Core.String.equal x y) h v then true else
	 list_list_mem t v
 *)    
  let test1 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:1 ~data:"one";
      bim#add ~key:2 ~data:"two";
      assert_equal 2 (bim#length);
      assert_equal false (bim#is_empty);
      assert_equal "one" (bim#find_exn ~key:1);
      assert_equal "two" (bim#find_exn ~key:2);
      assert_equal 1 (bim#find_exn_inverse ~key:"one");
      assert_equal 2 (bim#find_exn_inverse ~key:"two");

      bim#add_inverse ~key:"three" ~data:3;

      assert_equal "three" (bim#find_exn ~key:3);
      assert_equal 3 (bim#find_exn_inverse ~key:"three");
      assert_equal [(1,"one");(2,"two");(3,"three")] (bim#to_alist ());
      assert_equal [(3,"three");(2,"two");(1,"one")] (bim#to_alist ~key_order:`Decreasing ());
      
      bim#change ~key:3 ~f:(fun x -> (Some "tri"));

      assert_equal "tri" (bim#find_exn ~key:3);
      assert_equal 3 (bim#find_exn_inverse ~key:"tri");      
      bim#change_inverse ~key:"tri" ~f:(fun x -> (Some 4));
      assert_equal 4 (bim#find_exn_inverse ~key:"tri");
      assert_equal "tri" (bim#find_exn ~key:4);
      bim#remove ~key:1;
      assert_equal 2 (bim#length);
      bim#remove_inverse ~key:"two";
      assert_equal 1 (bim#length);
      bim#remove ~key:4;
      assert_equal 0 (bim#length);     
    end

  let test2 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:1 ~data:"one";
      bim#add ~key:2 ~data:"two";
      assert_equal 2 (bim#length);
      assert_equal false (bim#is_empty);
      assert_equal (Some "one") (bim#find ~key:1);
      assert_equal (Some "two") (bim#find ~key:2);
      assert_equal (Some 1) (bim#find_inverse ~key:"one");
      assert_equal (Some 2) (bim#find_inverse ~key:"two");
      assert_equal None (bim#find ~key:3);
      assert_equal None (bim#find_inverse ~key:"three");
      bim#add_inverse ~key:"three" ~data:3;
      assert_equal (Some "three") (bim#find ~key:3);
      assert_equal (Some 3) (bim#find_inverse ~key:"three");

      bim#change ~key:3 ~f:(fun x -> (Some "tri"));

      assert_equal 3 (bim#find_exn_inverse ~key:"tri");

      bim#change_inverse ~key:"tri" ~f:(fun x -> (Some 4));

      assert_equal (Some 4) (bim#find_inverse ~key:"tri");
      assert_equal (Some "tri") (bim#find ~key:4);
      assert_equal true (bim#mem 1);
      assert_equal true (bim#mem 2);
      assert_equal true (bim#mem_inverse "tri");
      assert_equal (Some (1,"one")) (bim#min_elt);
      assert_equal (Some (4,"tri")) (bim#max_elt);
      assert_equal (1,"one") (bim#min_elt_exn);
      assert_equal (4,"tri") (bim#max_elt_exn);
      assert_equal (Some ("one",1)) (bim#min_elt_inverse);
      assert_equal (Some ("two",2)) (bim#max_elt_inverse);
      assert_equal ("one",1) (bim#min_elt_exn_inverse);
      assert_equal ("two",2) (bim#max_elt_exn_inverse);
      assert_equal (Some (2,"two")) (bim#nth 1);
      assert_equal (Some (4,"tri")) (bim#nth 2);
      assert_equal (Some ("one",1)) (bim#nth_inverse 0);
      assert_equal (Some ("tri",4)) (bim#nth_inverse 1);
      assert_equal (Some ("two",2)) (bim#nth_inverse 2);
      assert_equal None (bim#nth_inverse 3);
    end

  let test3 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:1 ~data:"single";
      bim#add ~key:2 ~data:"double";
      bim#add ~key:3 ~data:"triple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#length);
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 2 (bim#counti ~f:(fun ~key ~data -> if key <= 2 then true else false));
      assert_equal ["single";"double";"triple"] (bim#data);
      assert_equal true (List.mem 1 (bim#data_inverse));
      assert_equal true (List.mem 2 (bim#data_inverse));
      assert_equal true (List.mem 3 (bim#data_inverse));
      assert_equal true (bim#exists ~f:(fun x -> x = "double"));
      assert_equal true (bim#exists ~f:(fun x -> x = "triple"));
      assert_equal false (bim#exists ~f:(fun x -> x = "quadruple"));
      assert_equal true (bim#exists_inverse ~f:(fun x -> x = 2));
      assert_equal false (bim#exists_inverse ~f:(fun x -> x = 4));
      assert_equal true (bim#existsi ~f:(fun ~key ~data -> key = 3 && data = "triple"));
      assert_equal true (bim#existsi_inverse ~f:(fun ~key ~data -> key = "double" && data = 2));
      bim#empty ();
      assert_equal 0 (bim#length);
      assert_equal 0 (bim#count ~f:(fun x -> true));
      assert_equal true (bim#is_empty);

      bim#add ~key:1 ~data:"one";
      bim#add ~key:2 ~data:"two";
      bim#add ~key:3 ~data:"three";
      bim#update 1 ~f:(fun x -> match x with
				  Some s -> Core.String.rev s
				 | None -> "newentry");
      assert_equal "eno" (bim#find_exn ~key:1);
    end 

  let test3 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#length);
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filter ~f:(fun v -> String.length v > 8);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));

      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filter_inverse ~f:(fun v -> v > 4);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      bim#add ~key:4 ~data:"quadruple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filter_keys ~f:(fun k -> k < 5);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filter_keys_inverse ~f:(fun v -> String.length v > 8);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));

      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filteri ~f:(fun ~key ~data -> String.length data > 8 || key <6);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal (Some "quadruple") (bim#find ~key:4);
      assert_equal (Some "pentuple") (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      
      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      bim#filteri_inverse ~f:(fun ~key ~data -> String.length key > 8 || data <6);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal 2 (bim#count_inverse ~f:(fun x -> true));
      assert_equal (Some "quadruple") (bim#find ~key:4);
      assert_equal (Some "pentuple") (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some 4) (bim#find_inverse ~key:"quadruple");
      assert_equal (Some 5) (bim#find_inverse ~key:"pentuple");
      assert_equal None (bim#find_inverse ~key:"sextuple");

      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      bim#filter_map ~f:(fun v -> if String.length v > 8 then (Some "survivor") else None);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));
      assert_equal (Some "survivor") (bim#find ~key:4);
      assert_equal None (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some 4) (bim#find_inverse ~key:"survivor");
      assert_equal None (bim#find_inverse ~key:"pentuple");
      assert_equal None (bim#find_inverse ~key:"sextuple");

      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      bim#filter_map_inverse ~f:(fun v -> if v > 5 then (Some (v * 2)) else None);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));
      assert_equal (Some 12) (bim#find_inverse ~key:"sextuple");
      assert_equal None (bim#find_inverse ~key:"quadruple");
      assert_equal None (bim#find_inverse ~key:"pentuple");
      assert_equal None (bim#find ~key:4);
      assert_equal None (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some "sextuple") (bim#find ~key:12);      
    end

  let test4 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:1 ~data:"single";
      bim#add ~key:2 ~data:"double";
      bim#add ~key:3 ~data:"triple";
      let concat = bim#fold ~init:"" ~f:(fun ~key ~data init -> if key < 3 then (init ^ data) else init) in
      assert_equal "singledouble" concat;
      let sum = bim#fold_inverse
		      ~init:0
		      ~f:(fun ~key ~data init ->
			  if key.[0] = 't' || key.[0] = 'd'
			  then (data + init) else init) in
      assert_equal 5 sum;
      let dividend = bim#fold_inverse
		       ~init:1
		       ~f:(fun ~key ~data init ->
			   if key.[0] = 't' || key.[0] = 'd'
			   then (data / init) else init) in
      assert_equal 1 dividend;
      let concat2 =
	bim#fold_right
	      ~init:"" ~f:(fun ~key ~data init ->
			   if key < 3 then (init ^ data) else init) in
      assert_equal "doublesingle" concat2;
      let dividend2 =
	bim#fold_right_inverse
			 ~init:1 ~f:(fun ~key ~data init ->
				     if key.[0] ='t' || key.[0]='d'
				     then (data / init) else init) in
      assert_equal 0 dividend2;
      assert_equal true (bim#for_all ~f:(fun v -> if (String.length v) > 0 then true else false));
    end

  let test5 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:1 ~data:"uno";
      bim#add ~key:2 ~data:"dos";
      bim#add ~key:3 ~data:"tres";
      assert_equal true (bim#for_all ~f:(fun v -> (String.length v) > 1));
      assert_equal false (bim#for_all ~f:(fun v -> (String.length v) > 4));
      assert_equal true (bim#for_all_inverse ~f:(fun v -> v < 4));
      assert_equal false (bim#for_all_inverse ~f:(fun v -> v > 3));
      assert_equal [1;2;3] (bim#keys);
      assert_equal ["uno";"dos";"tres"] (bim#data);
      bim#map ~f:(fun v -> Core.String.concat [v;v;]);
      assert_equal ["unouno";"dosdos";"trestres"] (bim#data);
      bim#mapi ~f:(fun ~key ~data -> Core.String.concat [(Core.Int.to_string key);"->";data]);
      assert_equal ["1->unouno";"2->dosdos";"3->trestres"] (bim#data);
      assert_equal "1->unouno" (bim#find_exn ~key:1);
      assert_equal "2->dosdos" (bim#find_exn ~key:2);
      assert_equal 1 (bim#find_exn_inverse ~key:"1->unouno");
      assert_equal 2 (bim#find_exn_inverse ~key:"2->dosdos");

      bim#map_inverse ~f:(fun x -> x*2);
      assert_equal [2;4;6] (bim#keys);
      bim#mapi_inverse ~f:(fun ~key ~data -> if key.[0]='1' then
					       data*1
					     else
					       if key.[0]='2' then
						 data*2
					       else
						 data*3);
      assert_equal [2;8;18] (bim#keys);
    end

  let test6 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap_multi.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";
      bim#add_multi_inverse ~key:"tri" ~data:3;
      bim#add_multi_inverse ~key:"four" ~data:4;
      (*==todo change change_inverse*)
      assert_equal 4 (bim#count ~f:(fun l -> true));
      assert_equal 4 (bim#counti ~f:(fun ~key ~data -> true));
      assert_equal 1 (bim#counti ~f:(fun ~key ~data -> key > 3));
      assert_equal 8 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 4 (bim#length);
      assert_equal 1 (List.length (bim#find_exn 1));
      assert_equal 2 (List.length (bim#find_exn 2));
      assert_equal 4 (List.length (bim#find_exn 3));
      assert_equal 1 (List.length (bim#find_exn 4));
      assert_equal true (List.mem "one" (bim#find_exn 1));
      assert_equal true (List.mem "two" (bim#find_exn 2));
      assert_equal true (List.mem "dos" (bim#find_exn 2));
      assert_equal true (List.mem "three" (bim#find_exn 3));
      assert_equal true (List.mem "tres" (bim#find_exn 3));
      assert_equal true (List.mem "triple" (bim#find_exn 3));
      assert_equal true (List.mem "tri" (bim#find_exn 3));
      
      assert_equal 1 (bim#find_exn_inverse "one");
      assert_equal 2 (bim#find_exn_inverse "two");
      assert_equal 2 (bim#find_exn_inverse "dos");
      assert_equal 3 (bim#find_exn_inverse "three");
      assert_equal 3 (bim#find_exn_inverse "tres");
      assert_equal 3 (bim#find_exn_inverse "triple");
      assert_equal 3 (bim#find_exn_inverse "tri");

      assert_equal (Some 1) (bim#find_inverse "one");
      assert_equal (Some 2) (bim#find_inverse "two");
      assert_equal (Some 2) (bim#find_inverse "dos");
      assert_equal (Some 3) (bim#find_inverse "three");
      assert_equal (Some 3) (bim#find_inverse "tres");
      assert_equal (Some 3) (bim#find_inverse "triple");
      assert_equal 3 (bim#find_exn_inverse "tri");
      
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);*)
      assert_equal ([["one"];["dos";"two"];["tri";"triple";"tres";"three"];["four"]]) (bim#data);
      (*print_n_flush_alist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_inverse);*)
      assert_equal 1 (List.length (Core.List.filter (bim#data_inverse) ~f:(fun x -> if x = 1 then true else false)));
      assert_equal 2 (List.length (Core.List.filter (bim#data_inverse) ~f:(fun x -> if x = 2 then true else false)));
      assert_equal 4 (List.length (Core.List.filter (bim#data_inverse) ~f:(fun x -> if x = 3 then true else false)));
      assert_equal 1 (List.length (Core.List.filter (bim#data_inverse) ~f:(fun x -> if x = 4 then true else false)));

      bim#empty ();
      assert_equal 0 (bim#count ~f:(fun l -> true));
      assert_equal 0 (bim#counti ~f:(fun ~key ~data -> true));
      assert_equal 0 (bim#counti ~f:(fun ~key ~data -> key > 3));
      assert_equal 0 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 0 (bim#length);

      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";
      bim#add_multi_inverse ~key:"tri" ~data:3;

      assert_equal true (bim#exists ~f:(fun x -> List.length x = 1 && Core.String.equal (List.nth x 0) "one"));
      assert_equal true (bim#exists_inverse ~f:(fun x -> x = 2));
      assert_equal true (bim#existsi ~f:(fun ~key ~data -> List.length data = 2 && key = 2 && List.mem "two" data));
      assert_equal false (bim#existsi ~f:(fun ~key ~data -> List.mem "one" data && key = 2));
      assert_equal true (bim#existsi_inverse ~f:(fun ~key ~data -> key = "tri" && data=3));

      bim#change ~key:3 ~f:(fun x -> (Some ["iii";"tri";"triple";"tres";"three"]));
      assert_equal (Some 3) (bim#find_inverse "three");
      assert_equal (Some 3) (bim#find_inverse "tres");
      assert_equal (Some 3) (bim#find_inverse "tri");
      assert_equal 3 (bim#find_exn_inverse "triple");
      assert_equal 3 (bim#find_exn_inverse "iii");
      assert_equal 5 (List.length (bim#find_exn 3));

      bim#change_inverse ~key:"two" ~f:(fun x -> (Some 4));
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);
      print_n_flush_alist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_inverse);*)
      assert_equal 2 (List.length (bim#find_exn ~key:4));
      assert_equal true (List.mem "two" (bim#find_exn ~key:4)); 
      assert_equal true (List.mem "dos" (bim#find_exn ~key:4));
      assert_equal 4 (bim#find_exn_inverse ~key:"two");
      assert_equal 4 (bim#find_exn_inverse ~key:"dos");
      assert_equal None (bim#find ~key:2);

    end 

  let test7 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap_multi.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter ~f:(fun v -> (List.length v) < 3);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 2 (bim#length);

      bim#filter_inverse ~f:(fun k -> k < 2);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);
    end

  let test8 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap_multi.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filteri ~f:(fun ~key ~data -> key < 3 && (List.length data) > 1);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 2 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filteri_inverse ~f:(fun ~key ~data -> key.[0] = 't' && data < 3);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_keys ~f:(fun k -> k > 2);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_keys_inverse ~f:(fun k -> (String.length k) > 3);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_map ~f:(fun vlist -> if List.length vlist < 3 then Some vlist else None);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 2 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_map_inverse ~f:(fun invk -> if invk < 3 then Some (invk+1) else None);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 2 (bim#length);
      assert_equal 1 (List.length (bim#find_exn ~key:2));
      assert_equal 2 (List.length (bim#find_exn ~key:3));
      assert_equal true (List.mem "one" (bim#find_exn ~key:2));
      assert_equal true (List.mem "two" (bim#find_exn ~key:3)); 
      assert_equal true (List.mem "dos" (bim#find_exn ~key:3));
      assert_equal 2 (bim#find_exn_inverse ~key:"one");
      assert_equal 3 (bim#find_exn_inverse ~key:"two");
      assert_equal 3 (bim#find_exn_inverse ~key:"dos");
      assert_equal None (bim#find ~key:1);
    end 


  let test9 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap_multi.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      let total_chars =
	bim#fold ~init:0 ~f:(fun ~key ~data accum ->
			     (Core.List.fold data ~init:accum
					     ~f:(fun accum data -> (String.length data) + accum))) in
      assert_equal 24 total_chars;
      let keys_times_num_values =
	bim#fold_inverse ~init:0 ~f:(fun ~key ~data accum ->
				     accum + data) in
      assert_equal 14 keys_times_num_values;
      (*fold over keys in decreasing order instead of increasing--in this case reverse alphabetical order*)
      let folded_right =
	bim#fold_right ~init:None
		       ~f:(fun ~key ~data accum ->
			   match accum with
			   | None -> Some key
			   | Some i -> Some (i - key)
			  ) in
      assert_equal (Some 0) folded_right;
      let folded_right_inverse = bim#fold_right_inverse
				       ~init:[]
				       ~f:(fun ~key ~data accum ->
					   key::accum
					  ) in
      (*print_n_flush_alist ~sep:"|" ~to_string_func:(fun x -> x) folded_right_inverse;*)
      assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_inverse) 0);
      let forall = bim#for_all
			 ~f:(fun l ->
			     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 3))
			    ) in
      assert_equal forall false;
      assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_inverse) 0);
      let forall = bim#for_all
			 ~f:(fun l ->
			     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 2))
			    ) in
      assert_equal forall true;
      let forallinverse = bim#for_all_inverse ~f:(fun x -> x < 4) in
      assert_equal forallinverse true;
      let forallinverse = bim#for_all_inverse ~f:(fun x -> x < 3) in
      assert_equal forallinverse false;
      assert_equal false (bim#is_empty);
      assert_equal [1;2;3] (bim#keys);
      (*let () print_n_flush_alist ~sep:"," ~to_string_func:(fun x -> x) inverse_keys in *)
      assert_equal true (List.mem "dos" (bim#keys_inverse));
      assert_equal 1 (List.length (List.filter (fun x -> String.equal x "dos") (bim#keys_inverse)));
      assert_equal true (List.mem "one" (bim#keys_inverse));
      assert_equal true (List.mem "three" (bim#keys_inverse));
      assert_equal true (List.mem "tres" (bim#keys_inverse));
      assert_equal true (List.mem "triple" (bim#keys_inverse));
      assert_equal true (List.mem "two" (bim#keys_inverse));

      assert_equal 3 (bim#length);

      bim#map ~f:(fun l -> let len = List.length l in
			   match len with
			   | 1 -> ["1"]@l
			   | 2 -> ["2"]@l
			   | 3 -> ["3"]@l
			   | _ -> ["unexpectedlen"]@l);
      assert_equal 2 (List.length (bim#find_exn 1));
      assert_equal true (List.mem "1" (bim#find_exn 1));
      assert_equal 3 (List.length (bim#find_exn 2));
      assert_equal true (List.mem "2" (bim#find_exn 2));
      assert_equal 4 (List.length (bim#find_exn 3));
      assert_equal true (List.mem "3" (bim#find_exn 3));
      assert_equal 1 (bim#find_exn_inverse "1");
      assert_equal 2 (bim#find_exn_inverse "2");
      assert_equal 3 (bim#find_exn_inverse "3");
      assert_equal 1 (bim#find_exn_inverse "one");
      assert_equal 2 (bim#find_exn_inverse "two");
      assert_equal 3 (bim#find_exn_inverse "three");
    end

  let test10 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap_multi.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      assert_equal true (bim#mem 1);
      assert_equal true (bim#mem 2);
      assert_equal true (bim#mem 3);
      assert_equal true (bim#mem_inverse "one");
      assert_equal true (bim#mem_inverse "two");
      assert_equal true (bim#mem_inverse "three");
      
      bim#map_inverse ~f:(fun x -> x *10);
      assert_equal 10 (bim#find_exn_inverse "one");
      assert_equal 20 (bim#find_exn_inverse "two");
      assert_equal 30 (bim#find_exn_inverse "three");
      assert_equal None (bim#find 1);
      assert_equal None (bim#find 2);
      assert_equal None (bim#find 3);
      assert_equal true (List.mem "one" (bim#find_exn 10));
      assert_equal true (List.mem "two" (bim#find_exn 20));
      assert_equal true (List.mem "three" (bim#find_exn 30));

      assert_equal (Some (10, ["one"])) (bim#min_elt);
      assert_equal (10, ["one"]) (bim#min_elt_exn);
      assert_equal (Some (30, ["triple";"tres";"three"])) (bim#max_elt);
      assert_equal (30,["triple";"tres";"three"]) (bim#max_elt_exn);

      assert_equal (Some ("dos", 20)) (bim#min_elt_inverse);
      assert_equal ("dos", 20) (bim#min_elt_exn_inverse);
      assert_equal (Some ("two", 20)) (bim#max_elt_inverse);
      assert_equal ("two",20) (bim#max_elt_exn_inverse);

      assert_equal (Some (10, ["one"])) (bim#nth 0);
      assert_equal (Some (20, ["two";"dos"])) (bim#nth 1);
      assert_equal (Some (30, ["triple";"tres";"three"])) (bim#nth 2);
      assert_equal (Some ("dos",20)) (bim#nth_inverse 0);
      assert_equal (Some ("one",10)) (bim#nth_inverse 1);
      assert_equal (Some ("three",30)) (bim#nth_inverse 2);
      assert_equal (Some ("tres",30)) (bim#nth_inverse 3);
      assert_equal (Some ("triple",30)) (bim#nth_inverse 4);
      assert_equal (Some ("two",20)) (bim#nth_inverse 5);

      bim#remove ~key:20;
      assert_equal None (bim#find 20);
      assert_equal None (bim#find_inverse "two");

      bim#remove_inverse ~key:"triple";
      assert_equal false (List.mem "triple" (bim#find_exn 30));
      assert_equal true (List.mem "three" (bim#find_exn 30));
      assert_equal true (List.mem "tres" (bim#find_exn 30));

      bim#remove_multi ~key:30;
      assert_equal false (List.mem "triple" (bim#find_exn 30));
      assert_equal false (List.mem "tres" (bim#find_exn 30));
      assert_equal true (List.mem "three" (bim#find_exn 30));
      assert_equal 1 (List.length (bim#find_exn 30));
      assert_equal None (bim#find_inverse "tres");
      assert_equal (Some 30) (bim#find_inverse "three");

(*      bim#update 30 ~f:(fun l ->
			match l with
			| Some elems -> 
			| None ->
		       )*)
    end 

  let suite =
    "suite">:::
      ["test1">:: test1;
       "test2">:: test2;
       "test3">:: test3;
       "test4">:: test4;
       "test5">:: test5;
       "test6">:: test6;
       "test7">:: test7;
       "test8">:: test8;
       "test9">:: test9;
       "test10">:: test10];;

  let () =
    run_test_tt_main suite
	
end 
