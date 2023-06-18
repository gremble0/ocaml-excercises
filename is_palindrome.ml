let rev (list: 'a list): 'a list = 
    let rec rev' (list: 'a list) (builder: 'a list): 'a list =
        match list with
        | [] -> builder
        | x :: rest -> rev' rest (x :: builder)
    in rev' list []

let is_palindrome (list: 'a list): bool = 
    let reversed_list = rev list in
    if list = reversed_list
        then true
        else false

let () = 
    assert (is_palindrome [1; 2; 3] = false);
    assert (is_palindrome ["a"; "b"; "a"] = true);
    assert (is_palindrome [1] = true);
    assert (is_palindrome [] = true);
