(*Could also use our custom made list reverse*)
let is_palindrome (list: 'a list) = list = List.rev list

let () = 
    assert (is_palindrome [1; 2; 3] = false);
    assert (is_palindrome ["a"; "b"; "a"] = true);
    assert (is_palindrome [1] = true);
    assert (is_palindrome [] = true);
