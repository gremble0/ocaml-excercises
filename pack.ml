let pack (list: 'a list): 'a list list =
    let rec pack' (list: 'a list) (builder: 'a list): 'a list list =
        match list with
        | [] -> [builder]
        | y :: rest ->
            if List.hd builder = y 
            then pack' rest (y :: builder)
            else builder :: pack' rest [y] 
    in match list with
    | [] -> []
    | x :: list -> pack' list [x] 

let () =
    assert (pack [1; 2; 3] = [[1]; [2]; [3]]);
    assert (pack [1; 1; 2; 2; 3; 3] = [[1; 1]; [2; 2]; [3; 3]]);
    assert (pack [] = []);
    assert (pack [1] = [[1]]);
