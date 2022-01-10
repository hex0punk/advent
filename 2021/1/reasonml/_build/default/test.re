let file_in_channel = open_in("input.txt")

let file_stream =
  Stream.from(_i => {
    switch(input_line(file_in_channel)) {
    | line => Some(line)
    | exception(End_of_file) => None
    };
  });


let process = (fs): unit => {
  let rec count_incs = (acc: int, prev: int) : int => {
     switch(Stream.next(fs)) {
     | depth => {
         let depth_int = int_of_string(depth)
         switch (depth_int > prev) {
            | true => count_incs(acc + 1, depth_int)
	    | false => count_incs(acc, depth_int)
	 }
       }
     | exception Stream.Failure => acc
     }
  }
  let result = count_incs(0, 0);
  print_int(result - 1)
}

file_stream |> process
