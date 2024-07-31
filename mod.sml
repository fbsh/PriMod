(* mod.sml *)
structure mod = struct

  (* Exception for mod-specific errors *)
  exception modError of string

  (* Parse a string into an AST *)
  fun parse_string (s : string) : Syntax.module =
    let
      val tokens = Lexer.lexString s
      val ast = Parser.parse tokens
    in
      ast
    handle 
      Lexer.LexError msg => raise modError ("Lexer error: " ^ msg)
    | Parser.ParseError msg => raise modError ("Parser error: " ^ msg)
    end

  (* Parse a file into an AST *)
  fun parse_file (filename : string) : Syntax.module =
    let
      val inStream = TextIO.openIn filename
      val content = TextIO.inputAll inStream
      val _ = TextIO.closeIn inStream
    in
      parse_string content
    handle 
      IO.Io {name, ...} => raise modError ("IO error: Cannot open file " ^ name)
    end

  (* Elaborate an AST *)
  fun elaborate (ast : Syntax.module) : Internal.asig * Internal.purity =
    Elaboration.elaborate ast
    handle 
      Elaboration.ElaborationError msg => raise modError ("Elaboration error: " ^ msg)

  (* Compile a file: parse, elaborate, and return the result *)
  fun compile_file (filename : string) : Internal.asig * Internal.purity =
    let
      val ast = parse_file filename
      val (asig, purity) = elaborate ast
    in
      (asig, purity)
    end

  (* Pretty print an abstract signature *)
  fun pp_asig (asig : Internal.asig) : string =
    Internal.show_asig asig

  (* Pretty print purity *)
  fun pp_purity (purity : Internal.purity) : string =
    Internal.Purity.show purity

  (* Run the entire compilation process and print the result *)
  fun run_compile (filename : string) : unit =
    let
      val (asig, purity) = compile_file filename
    in
      print "Compilation successful.\n";
      print ("Abstract Signature:\n" ^ pp_asig asig ^ "\n");
      print ("Purity: " ^ pp_purity purity ^ "\n")
    end
    handle modError msg => 
      print ("Compilation failed: " ^ msg ^ "\n")

  (* Additional utility functions *)

  (* Check if a file exists *)
  fun file_exists (filename : string) : bool =
    (OS.FileSys.fileSize filename; true)
    handle OS.SysErr _ => false

  (* Get the contents of a file as a string *)
  fun read_file (filename : string) : string =
    let
      val inStream = TextIO.openIn filename
      val contents = TextIO.inputAll inStream
      val _ = TextIO.closeIn inStream
    in
      contents
    end

  (* Write a string to a file *)
  fun write_file (filename : string) (content : string) : unit =
    let
      val outStream = TextIO.openOut filename
      val _ = TextIO.output (outStream, content)
      val _ = TextIO.closeOut outStream
    in
      ()
    end

  (* Get the directory part of a file path *)
  fun get_directory (path : string) : string =
    OS.Path.dir path

  (* Join two path components *)
  fun join_paths (path1 : string) (path2 : string) : string =
    OS.Path.joinDirFile {dir = path1, file = path2}

  (* List files in a directory *)
  fun list_files (dir : string) : string list =
    let
      val dirStream = OS.FileSys.openDir dir
      fun loop acc =
        case OS.FileSys.readDir dirStream of
          NONE => acc
        | SOME file => loop (file :: acc)
      val files = loop []
      val _ = OS.FileSys.closeDir dirStream
    in
      files
    end

  (* Compile multiple files *)
  fun compile_files (filenames : string list) : unit =
    List.app (fn filename =>
      (print ("Compiling " ^ filename ^ "...\n");
       run_compile filename;
       print "\n"))
      filenames

  (* Main function to be called from the command line *)
  fun main (args : string list) : unit =
    case args of
      [] => print "Usage: mod <filename> [<filename2> ...]\n"
    | filenames => compile_files filenames

end