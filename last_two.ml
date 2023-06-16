let rec last_two (list: 'a list): ('a * 'a) option =
    match list with
    | [] -> None
    | [element] -> None
    | [element1; element2] -> Some (element1, element2)
    | _ :: rest -> last_two rest

let () =
    assert (last_two [1; 2; 3] = Some (2, 3));
    assert (last_two ["ab"; "cd"; "ef"; "gh"] = Some ("ef", "gh"));
    assert (last_two [1] = None);
    assert (last_two [] = None);
