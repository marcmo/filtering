require 'rake/clean'
require 'fileutils'
require 'benchmark'

Program="filter"
Executable = "#{Program}"

MainHs="Main.hs"

CLEAN.include("tmp","**/*.o","**/*.hi","dist","*.exe","#{Executable}")
SrcFiles = FileList.new('*.hs')

file Executable => SrcFiles do
  sh "ghc -O2 -o #{Program} -outputdir tmp --make #{MainHs} -i.. -fforce-recomp"
end

desc "run program on data"
file :run => Executable do
	sh "time #{Executable}"
end
task :build => [Executable] do
  FileUtils.rm_rf 'tmp', :verbose => true
end

task :default => [:clean, :build]
