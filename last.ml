let rec last (list: 'a list): 'a option = 
    match list with
    | [] -> None
    | [element] -> Some element
    | _ :: rest -> last rest

let () = 
    assert (last [1; 2; 3] = Some 3);
    assert (last ["a"] = Some "a");
    assert (last [] = None)
