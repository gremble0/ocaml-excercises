let rev (list: 'a list): 'a list = 
    let rec rev' (list: 'a list) (builder: 'a list): 'a list =
        match list with
        | [] -> builder
        | x :: rest -> rev' rest (x :: builder)
    in rev' list []

let () = 
    assert (rev [1; 2; 3] = [3; 2; 1]);
    assert (rev ["ab"; "cd"; "ef"; "gh"] = ["gh"; "ef"; "cd"; "ab"]);
    assert (rev [1] = [1]);
    assert (rev [] = []);
