let file_in_channel = open_in("input.txt")

type bin_counter = {
  ones: array(string),
  zeroes: array(string),
};

let file_stream =
  Stream.from(_i => {
    switch(input_line(file_in_channel)) {
    | line => Some(line)
    | exception(End_of_file) => None
    };
  });

let to_bin = Array.fold_left((res, item) => res ++ item, "0b")

let update_arrs = (ones, zeroes, bitval, line) => {
  if (bitval == '1'){
    let ones = Array.append(ones, [|line|]);
    { ones, zeroes };
  } else {
    let zeroes = Array.append(zeroes, [|line|]);
    { ones, zeroes };
  }
}

let rec process_arr = (bc, idx, pos, lines, crit) : array(string) => {
  let bc = update_arrs(bc.ones, bc.zeroes, lines[idx].[pos], lines[idx])
  if (Array.length(bc.ones) + Array.length(bc.zeroes) == Array.length(lines)){
    crit(bc.ones, bc.zeroes)
  } else {
    process_arr(bc, idx+1, pos, lines, crit)
  } 
}

let calc = (fs): unit => {
  let rec array_from_stream = (items): array(string) => {
    switch(Stream.next(fs)){
      | line => Array.append(items, [|line|]) |> array_from_stream
      | exception Stream.Failure => items 
    };
  };
  
  let res_one = array_from_stream([||])
  let bc = {ones:[||], zeroes:[||]}

  let rec process = (h, pos, crit) : array(string) => {
    switch (pos < 12) {
      | true => {
        let h = process_arr(bc, 0, pos, h, crit)
        Array.length(h) == 1 ? h : process(h, pos+1, crit)
      }
      | false => h
    };
  }

  let crit_high = (ones, zeroes) => Array.length(ones) >= Array.length(zeroes) ? ones : zeroes
  let crit_lows = (ones, zeroes) => Array.length(ones) >= Array.length(zeroes) ? zeroes : ones
  
  let x = crit_high 
    |> process(res_one,0)
    |> to_bin 
    |> Int32.of_string
    |> Int32.to_int


  let y = crit_lows
    |> process(res_one,0)
    |> to_bin 
    |> Int32.of_string
    |> Int32.to_int

  print_int(x * y)
}

file_stream |> calc
