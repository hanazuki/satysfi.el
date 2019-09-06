#!/usr/bin/env ruby

require 'tempfile'

$testfiles = ARGV.size > 0 ? ARGS : %w[indent/*.in.saty]

def emacs(*args)
  p cmd = %W[#{ENV.fetch('EMACS', 'emacs')} --batch --no-site-file -L #{File.dirname(__dir__)}] + args
  system(*cmd) or fail $?.to_s
end

Dir[*$testfiles].each do |file|
  tmpfile = Tempfile.create(['', '.saty'])

  begin

    emacs('--eval', <<EOS.chomp, file, tmpfile.path)
(pcase-let ((`(,fin ,fout) command-line-args-left))
  (require 'satysfi-mode)
  (satysfi-mode)
  (setq indent-tabs-mode nil)
  (insert-file-contents fin)
  (indent-region (point-min) (point-max))
  (write-region (point-min) (point-max) fout))
EOS

    system('diff', '-u', file.sub('.in.', '.out.'), tmpfile.path) or fail $?.to_s
  rescue
    raise
  else
    File.unlink(tmpfile)
  end
end
