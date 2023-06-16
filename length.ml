let length (list: 'a list): int = 
    let rec length' (list: 'a list) (counter: int): int = 
        match list with
        | [] -> counter
        | _ :: rest -> length' rest counter + 1
    in length' list 0

let () = 
    assert (length [1; 2; 3] = 3);
    assert (length ["ab"; "cd"; "ef"; "gh"] = 4);
    assert (length [] = 0)
