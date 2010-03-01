require 'rake/clean'
require 'benchmark'

Program="filtering"
Output="output.out"
# Input="sample.in"
# Input="C-small-practice.in"
# Input = "/Users/oliver/tmp/output.txt"
Input = "/home/omueller/shared/Y20LG1_CAN_Trace.asc"
ParallelOptions="+RTS -N2 -RTS"
Executable = "#{Program}"
ThreadedExecutable = "#{Program}_threaded"
ProgramCall="./#{Executable} #{Input}"
ProgramCallParallel="./#{ThreadedExecutable} #{ParallelOptions} #{Input}"

MainHs="filtering.hs"
TestDir="Test"
#Profiling
ProfilingExecutable = "for_profiling"
TimeProf="+RTS -p -K100M"      
StandardHeap="+RTS -hc -p -K100M" 
AllocationType="+RTS -hy -p -K100M" 
ConstructorAlloc="+RTS -hd -p -K100M" 
Profiling=TimeProf
# Profiling=StandardHeap
# Profiling=AllocationType
# Profiling=ConstructorAlloc
CLEAN.include(Output,"**/*.o","**/*.hi","dist","#{Program}.zip","*.exe","#{ProfilingExecutable}","#{Executable}","#{ThreadedExecutable}")
SrcFiles = FileList.new('*.hs')

file Executable => SrcFiles do
  sh "ghc -O2 -o #{Program} -outputdir tmp --make #{MainHs} -fforce-recomp"
end

file ThreadedExecutable => SrcFiles do
  sh "ghc -threaded -O2 -o #{Program}_threaded -outputdir tmp --make #{MainHs} -fforce-recomp"
end

file ProfilingExecutable => SrcFiles do
  sh "ghc -O2 -o #{ProfilingExecutable} -outputdir tmp --make #{MainHs} -prof -auto-all -caf-all -fforce-recomp"
end

desc "run threaded program on data"
file :threaded => ThreadedExecutable do
	sh "time #{ProgramCallParallel}"
end

desc "run program on data"
file :run => Executable do
	sh "time #{ProgramCall}"
end
task :build => [Executable]

desc "run all quickCheck testcases"
task :test do
  sh 'runhaskell Test/tests.hs'
end

desc "profiling"
task :prof => [:clean,ProfilingExecutable] do
  benchmark = Benchmark.realtime do
    sh "time ./#{ProfilingExecutable} #{Input} #{Profiling}"
  end
  puts "computing step took: " + sprintf("%.2f", benchmark)
  if Profiling!=TimeProf
    sh "hp2ps -e8in -c #{ProfilingExecutable}.hp"
  end
end

task :pack do
  sh "zip #{Program}.zip *.cabal *.hs *.rb README *.lhs"
end

task :default => [:clean, :build]
