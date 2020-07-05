:- use_module(library(readutil)).

% initialize_list(Size, Value, List) 
initialize_list(0, _, []) :- !.
initialize_list(Size, Value, [Value | RestOfList]) :-
	Size > 0,
	NewSize is Size - 1,
	initialize_list(NewSize, Value, RestOfList)
.

% add_at_index_with_8bit_overflow(List, Index, Value, ResultList)
add_at_index_with_8bit_overflow([Head | Tail], 0, Value, [NewHead | Tail]) :-
	NewHead is mod(Head + Value, 256)
.
add_at_index_with_8bit_overflow([Head | Tail], Index, Value, [Head | ResultTail]) :-
	Index > 0,
	NewIndex is Index - 1,
	add_at_index_with_8bit_overflow(Tail, NewIndex, Value, ResultTail)
.

% read_at_index(List, Index, Value)
read_at_index([Head | _], 0, Head) :- !.
read_at_index([_ | Tail], Index, Value) :-
	Index > 0,
	NewIndex is Index - 1,
	read_at_index(Tail, NewIndex, Value)
.

% store_at_index(List, Index, Value, ResultList)
store_at_index([_ | Tail], 0, Value, [Value | Tail]) :- !.
store_at_index([Head | Tail], Index, Value, [Head | ResultTail]) :-
	Index > 0,
	NewIndex is Index - 1,
	store_at_index(Tail, NewIndex, Value, ResultTail)
.

default_brainfuck_interpreter(Code) :-
	brainfuck_interpreter(Code, 1024)
.

% brainfuck_interpreter(Code, MemorySize)
brainfuck_interpreter(Code, MemorySize) :-
	initialize_list(MemorySize, 0, Memory),
	string_chars(Code, CodeList),
	execute_code(CodeList, Memory, 0, 0, []),
	!
.

execute_code(Code, _, InstructionPointer, _, _) :-
	length(Code, Length),
	InstructionPointer >= Length,
	!
.

execute_code(Code, Memory, InstructionPointer, MemoryPointer, Stack) :-
	read_at_index(Code, InstructionPointer, Instruction),
	execute_single_instruction(Instruction, Code, Memory, ResultMemory, InstructionPointer, ResultInstructionPointer, MemoryPointer, ResultMemoryPointer, Stack, ResultStack),
	execute_code(Code, ResultMemory, ResultInstructionPointer, ResultMemoryPointer, ResultStack)
.

execute_single_instruction('+', _, Memory, ResultMemory, InstructionPointer, ResultInstructionPointer, MemoryPointer, MemoryPointer, Stack, Stack) :-
	add_at_index_with_8bit_overflow(Memory, MemoryPointer, 1, ResultMemory),
	ResultInstructionPointer is InstructionPointer + 1,
	!
.

execute_single_instruction('-', _, Memory, ResultMemory, InstructionPointer, ResultInstructionPointer, MemoryPointer, MemoryPointer, Stack, Stack) :-
	add_at_index_with_8bit_overflow(Memory, MemoryPointer, -1, ResultMemory),
	ResultInstructionPointer is InstructionPointer + 1,
	!
.

execute_single_instruction('>', _, Memory, Memory, InstructionPointer, ResultInstructionPointer, MemoryPointer, ResultMemoryPointer, Stack, Stack) :-
	ResultMemoryPointer is MemoryPointer + 1,
	ResultInstructionPointer is InstructionPointer + 1,
	!
.

execute_single_instruction('<', _, Memory, Memory, InstructionPointer, ResultInstructionPointer, MemoryPointer, ResultMemoryPointer, Stack, Stack) :-
	ResultMemoryPointer is MemoryPointer - 1,
	ResultInstructionPointer is InstructionPointer + 1,
	!
.

execute_single_instruction('.', _, Memory, Memory, InstructionPointer, ResultInstructionPointer, MemoryPointer, MemoryPointer, Stack, Stack) :-
	read_at_index(Memory, MemoryPointer, Value),
	put_code(Value),
	flush_output,
	ResultInstructionPointer is InstructionPointer + 1,
	!
.

execute_single_instruction(',', _, Memory, ResultMemory, InstructionPointer, ResultInstructionPointer, MemoryPointer, MemoryPointer, Stack, Stack) :-
	get_single_char(Value),
	store_at_index(Memory, InstructionPointer, Value, ResultMemory),
	ResultInstructionPointer is InstructionPointer + 1,
	!
.

execute_single_instruction('[', _, Memory, Memory, InstructionPointer, ResultInstructionPointer, MemoryPointer, MemoryPointer, Stack, [InstructionPointer | Stack]) :-
	read_at_index(Memory, MemoryPointer, Value),
	Value > 0,
	!,
	ResultInstructionPointer is InstructionPointer + 1
.

execute_single_instruction('[', Code, Memory, Memory, InstructionPointer, ResultInstructionPointer, MemoryPointer, MemoryPointer, Stack, Stack) :-
	find_matching_bracket_right(Code, InstructionPointer, ResultInstructionPointer),
	!
.

execute_single_instruction(']', _, Memory, Memory, _, ResultInstructionPointer, MemoryPointer, MemoryPointer, [ResultInstructionPointer | Stack], Stack) :-
	read_at_index(Memory, MemoryPointer, Value),
	Value > 0,
	!
.

execute_single_instruction(']', _, Memory, Memory, InstructionPointer, ResultInstructionPointer, MemoryPointer, MemoryPointer, [_ | Stack], Stack) :-
	ResultInstructionPointer is InstructionPointer + 1,
	!
.

execute_single_instruction(_, _, Memory, Memory, InstructionPointer, ResultInstructionPointer, MemoryPointer, MemoryPointer, Stack, Stack) :-
	ResultInstructionPointer is InstructionPointer + 1
.

find_matching_bracket_right(Code, InstructionPointer, ResultInstructionPointer) :-
	find_matching_bracket_right_backend(Code, InstructionPointer, ResultInstructionPointer, 0)
.

% find_matching_bracket_right_backend(Code, InstructionPointer, ResultInstructionPointer, OpenedCounter)
find_matching_bracket_right_backend(Code, InstructionPointer, InstructionPointer, 1) :-
	read_at_index(Code, InstructionPointer, ']'),
	!
.

find_matching_bracket_right_backend(Code, InstructionPointer, ResultInstructionPointer, OpenedCounter) :-
	read_at_index(Code, InstructionPointer, '['),
	!,
	NewInstructionPointer is InstructionPointer + 1,
	NewOpenedCounter is OpenedCounter + 1,
	find_matching_bracket_right_backend(Code, NewInstructionPointer, ResultInstructionPointer, NewOpenedCounter)
.
find_matching_bracket_right_backend(Code, InstructionPointer, ResultInstructionPointer, OpenedCounter) :-
	read_at_index(Code, InstructionPointer, ']'),
	!,
	NewInstructionPointer is InstructionPointer + 1,
	NewOpenedCounter is OpenedCounter - 1,
	find_matching_bracket_right_backend(Code, NewInstructionPointer, ResultInstructionPointer, NewOpenedCounter)
.
find_matching_bracket_right_backend(Code, InstructionPointer, ResultInstructionPointer, OpenedCounter) :-
	NewInstructionPointer is InstructionPointer + 1,
	find_matching_bracket_right_backend(Code, NewInstructionPointer, ResultInstructionPointer, OpenedCounter)
.

check_for_call_arguments([_]) :- !.
check_for_call_arguments(_) :-
	write('Usage: brainlog <bffile>'),
	nl,
	halt
.

read_code_from_file(Filename, Code) :-
	access_file(Filename, read),
	open(Filename, read, File),
	read_stream_to_codes(File, CodeChars),
	string_chars(Code, CodeChars),
	close(File),
	!
.

read_code_from_file(Filename, _) :-
	write('Could not read file "'),
	write(Filename),
	write('"'),
	nl,
	halt
.

main :-
	current_prolog_flag(argv, Arguments),
	check_for_call_arguments(Arguments),
	Arguments = [Filename],
	read_code_from_file(Filename, Code),
	default_brainfuck_interpreter(Code),
	halt
.

?- main.

