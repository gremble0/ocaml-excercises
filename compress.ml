let compress (list: 'a list): 'a list = 
    let rec compress' (list: 'a list) (builder: 'a list) =
        match list with
        | [] -> builder
        | x :: rest -> if List.mem x builder 
            then compress' rest builder
            else compress' rest (x :: builder)
    in List.rev (compress' list [])

let () = 
    assert (compress [1;2;3] = [1;2;3]);
    assert (compress ["a";"b";"c";"c";"a"] = ["a";"b";"c"]);
    assert (compress [1] = [1]);
    assert (compress [] = []);
