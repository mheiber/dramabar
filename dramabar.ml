let default_columns = 40

(* terminal stuff *)

let get_terminal_columns () = let width, height = ANSITerminal.size() in width

let clear_line () =
    ANSITerminal.(
        set_cursor 1 (* row *) (-1) (* no change to col *);
        erase Eol;
    )

let render line =
    clear_line ();
    ANSITerminal.(print_string [] line);
    flush stdout;;


(* make the progress bar *)

let rec str_n_times str n =
    if n < 1 then "" else str ^ (str_n_times str (n - 1));;

(* progress expected to be between 0 and 1 *)
let string_of_progress progress =
    let total_width = get_terminal_columns () in
    let progress_width = max 0 (int_of_float (progress *. (float_of_int total_width))) in
    let goal_width = max 0 (total_width - progress_width) in
    (* use same characters as https://github.com/yarnpkg/yarn *)
    let progress_bar = str_n_times "█" progress_width in
    let remaining_bar = str_n_times "░" goal_width in
    progress_bar ^ remaining_bar


(* demo *)

let sleepf (sec: float) = ignore (Unix.select [] [] [] sec)

let rec main (progress: float) =
    render (string_of_progress progress);
    sleepf 0.05;
    if progress <= 1.0 then main (progress +. 0.05)

let () = main 0.;
