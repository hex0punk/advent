let file_in_channel = open_in("input.txt")

let file_stream =
  Stream.from(_i => {
    switch(input_line(file_in_channel)) {
    | line => Some(line)
    | exception(End_of_file) => None
    };
  });


let update_count = (line: string, counter: array(int)): array(int) => {
  line |> String.iteri((i, x) =>  {
    counter[i] = counter[i] + (x == '0' ? 0 : 1)
  })
  counter
}

let get_gamma_bin = (total: int, counter: array(int)): array(int) => {
  let calc_gamma_val = (item: int) : int => item <= (total / 2) ? 0 : 1 
  Array.map(calc_gamma_val, counter)
}

let to_bin = Array.fold_left((res, item)  => res ++ string_of_int(item), "0b")

let process = (fs): unit => {
  let rec diag = (num_lines: int, counter: array(int)): (array(int), int) => {
    switch(Stream.next(fs)){
      | line => update_count(line, counter) |> diag(num_lines + 1)
      | exception Stream.Failure => (counter, num_lines)
    }
  }
  let (counter, total) = diag(0, Array.make(12, 0))
  let gamma_bin = get_gamma_bin(total, counter)
  let gamma = gamma_bin 
    |> to_bin  
    |> Int32.of_string
    |> Int32.to_int

  let eps = gamma_bin
    |> Array.map((item: int) => item == 0 ? 1 : 0)
    |> to_bin
    |> Int32.of_string
    |> Int32.to_int
  
  print_int(gamma * eps)
}

file_stream |> process
