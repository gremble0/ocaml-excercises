let pack (list: 'a list): 'a list list =
    let rec pack' (list: 'a list) (builder: 'a list): 'a list list =
        match list with
        | [] -> [builder]
        | element :: rest ->
            if List.hd builder = element 
            then pack' rest (element :: builder)
            else builder :: pack' rest [element] 
    in match list with
    | [] -> []
    | x :: rest -> pack' rest [x] 

let () =
    assert (pack [1; 2; 3] = [[1]; [2]; [3]]);
    assert (pack [1; 1; 2; 2; 3; 3] = [[1; 1]; [2; 2]; [3; 3]]);
    assert (pack [] = []);
    assert (pack [1] = [[1]]);
