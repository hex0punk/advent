let file_in_channel = open_in("input2.txt")

let file_stream =
  Stream.from(_i => {
    switch(input_line(file_in_channel)) {
    | line => Some(line)
    | exception(End_of_file) => None
    };
  });

let slide_window = (arr: array(int), measure: int): array(int) => {
  [|arr[1],arr[2], measure|];
}

let process = (fs): unit => {
  let rec count_incs = (max_sum: int, acc: int, prev_window_sum: int, prev_list: array(int)) : int => {
    switch(Stream.next(fs)) {
      | depth => {
          let depth_int = int_of_string(depth);
          if (Array.length(prev_list) < 3) {
            Array.append(prev_list, [|depth_int|])
            |> count_incs(max_sum+depth_int, acc, max_sum+depth_int);
          } else {
            let window_sum = prev_window_sum + depth_int - prev_list[0];
            if (window_sum > prev_window_sum) {
              slide_window(prev_list, depth_int) 
              |> count_incs(window_sum, acc+1, window_sum);
            } else {
              slide_window(prev_list, depth_int) 
              |> count_incs(prev_window_sum, acc, window_sum);
            }
          }
       }
     | exception Stream.Failure => {
          acc
       }
     }
  }
  let init_state = [||]
  let result = count_incs(0, 0, 0, init_state);
  print_int(result)
}

file_stream |> process
