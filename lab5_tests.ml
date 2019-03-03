open Lab5;;

let unit_test (condition : bool) (msg : string) : unit =
  if condition then
    Printf.printf "%s passed\n" msg
  else
    Printf.printf "%s FAILED\n" msg ;;

(*Part I Unit Tests*)
let test_validated_rgb () =
	let c = RGB (12,12,100)  in
	unit_test ((validated_rgb c) = RGB (12,12,100)) "validated_rgb true";;


let test_make_color () =
	let c = RGB (12, 12, 100)  in
	unit_test (make_color (12, 12, 100) = RGB (12, 12, 100)) "make_color" ;;

let test_convert_to_rgb () =
	let simple_red = Simple Red in
	unit_test ((convert_to_rgb simple_red)
	           = (255, 0, 0)) "convert_to_rgb" ;;

(*Part II Unit Tests*)
let test_validated_date () =
	let d = {year = 2018; month = 3; day = 10} in
	unit_test((validated_date d)
				= {year=2018;month=3;day=10})
			"validated_date";;

(*Part III Unit Tests*)
let test_new_child () =
	let d = {year = 2018; month = 3; day = 10} in
	let child = {name = "Edgar";
			favorite = (Simple Red);
			birthdate = d} in
	let fam = (Single child) in
	unit_test ((new_child "Edgar" (Simple Red) d) = fam) "new_child" ;;

let test_marry () =
	let d = {year = 2018; month = 3; day = 10} in
	let member = {name = "Edgar";
			favorite = (Simple Red);
			birthdate = d} in
	let fam = (Single member) in
	let married_fam = (Family (member, member,[])) in
	unit_test ((marry fam member) = married_fam) "marry valid";;


let test_add_to_family () =
	let d = {year = 2018; month = 3; day = 10} in
	let member = {name = "Edgar";
			favorite = (Simple Red);
			birthdate = d} in
	let fam = (Single member) in
	let married_fam = (Family (member, member,[])) in
	unit_test ((add_to_family married_fam fam)
	           = Family (member, member, [fam])) "add_to_family" ;;

let test_count_people () =
	let d = {year = 2018; month = 3; day = 10} in
	let member = {name = "Edgar";
			favorite = (Simple Red);
			birthdate = d} in
	let fam = (Family (member, member,[])) in
	let big_fam = (add_to_family fam fam) in
	unit_test ((count_people big_fam) = 4) "count_family" ;;

let test_all () =
	test_validated_rgb ();
	test_make_color ();
	test_convert_to_rgb ();
	test_validated_date ();
	test_new_child ();
	test_marry ();
	test_add_to_family ();
	test_count_people ();;


let _ = test_all ();;