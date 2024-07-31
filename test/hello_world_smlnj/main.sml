structure Main =
struct
  fun main (progname, args) =
    (print "Hello, World!\n";
     OS.Process.success)
end

val _ = SMLofNJ.exportFn("main", Main.main)