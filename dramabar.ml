let default_columns = 40

let with_process_in cmd args f =
    let path = ["/bin";"/usr/bin"] in
    let cmd =
        List.find Sys.file_exists (List.map (fun d -> Filename.concat d cmd) path)
    in
    let ic = Unix.open_process_in (cmd^" "^args) in
    try
        let r = f ic in
        ignore (Unix.close_process_in ic) ; r
    with exn ->
        ignore (Unix.close_process_in ic) ; raise exn

let get_terminal_columns () = let width, height = ANSITerminal.size() in width


let rec str_n_times str n =
    if n < 2 then str else str ^ (str_n_times str (n - 1));;

let clear_line () =
    ANSITerminal.(
        set_cursor 1 (* row *) (-1) (* no change to col *);
        erase Eol;
    );;

let sleepf (sec: float) =
    ignore (Unix.select [] [] [] sec)

let render (progress : int) (goal: int) =
    let max_width = get_terminal_columns () in
    let percentage = (float_of_int progress) /. (float_of_int goal) in
    let width = int_of_float (percentage *. (float_of_int max_width)) in
    (*print_string "rendered"; print_int width;*)
    clear_line ();
    ANSITerminal.(print_string [magenta; on_white] ((str_n_times "⑄" (max (width - 8) 0)) ^ "🐫 "));
    flush stdout;;

let rec main progress goal =
    render progress goal;
    print_int progress;
    sleepf 0.1;
    if progress < 100 then
    main (progress + 1) goal;;

let () = main 0 100;
