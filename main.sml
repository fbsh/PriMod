(* main.sml *)
structure Main = struct

  fun printUsage () =
    print "Usage: mod-lite <filename>\n"

  fun main (progname, args) =
    case args of
      [filename] =>
        (mod.run_compile filename;
         OS.Process.success)
    | _ =>
        (printUsage ();
         OS.Process.failure)

  (* This is used by SML/NJ's Compilation Manager *)
  val _ = SMLofNJ.exportFn ("mod-lite", main)

end

(* This is used when compiling with MLton *)
val _ = 
  case CommandLine.arguments () of
    [filename] => 
      (mod.run_compile filename;
       OS.Process.exit OS.Process.success)
  | _ => 
      (Main.printUsage ();
       OS.Process.exit OS.Process.failure)