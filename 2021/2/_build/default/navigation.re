open Str

let file_in_channel = open_in("input.txt")

let file_stream =
  Stream.from(_i => {
    switch(input_line(file_in_channel)) {
    | line => Some(line)
    | exception(End_of_file) => None
    };
  });


type position = (int, int)
type instruction = (string, int) 

let set_pos = (ins: instruction, pos: position) : position => {
  let (old_f, old_d) = pos
  switch(ins){
    | ("forward", num) => (old_f+num, old_d)
    | ("down", num) => (old_f, old_d+num)
    | ("up", num) => (old_f, old_d-num)
    | _ => pos
  }
}

let get_new_pos = (step: string, pos: position) : position => {
  let new_pos = regexp(" ") |> split(_, step)
  switch(new_pos){
    | [ins, num, ..._] => set_pos((ins, int_of_string(num)), pos) 
    | _ => pos
  }
}

let process = (fs): unit => {
  let rec navigate = (pos: position): position => {
    switch(Stream.next(fs)){
      | ins => get_new_pos(ins, pos) |> navigate
      | exception Stream.Failure => pos
    }
  }
  let (f, d) = navigate((0,0))
  print_int(f*d)
}

file_stream |> process
