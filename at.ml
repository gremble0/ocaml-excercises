let rec at (list: 'a list) (n: int): 'a option =
    (* Excercise wants function to start counting from 1 *)
    match list with
    | element :: _ when n == 1 -> Some element
    | _ :: rest when n > 1 -> at rest (n - 1)
    | _ -> None

let () =
    assert (at [1; 2; 3; 4] 2 = Some 2);
    assert (at ["ab"; "cd"; "ef"; "gh"] 1 = Some "ab");
    assert (at [1; 2; 3] 0 = None);
    assert (at [] 1 = None);
