require 'rubygems'

# Save loads of history
IRB.conf[:SAVE_HISTORY] = 10000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"

# Load the readline module.
IRB.conf[:USE_READLINE] = true

# Remove the annoying irb(main):001:0 and replace with >>
IRB.conf[:PROMPT_MODE]  = :SIMPLE

# Tab Completion
require 'irb/completion'

# Automatic Indentation
IRB.conf[:AUTO_INDENT]=true

# Table-ise results, etc
begin
  require 'hirb'
  Hirb.enable
  extend Hirb::Console
rescue LoadError
  puts $!
end

# # "3".what?(3) lists all methods that return 3
begin
  require "what_methods"
rescue LoadError
  puts $!
end

# 'lp' to show method lookup path
begin
  require 'looksee/shortcuts'
rescue LoadError
  puts $!
end

# Easily edit and run code snippets
#require 'sketches'
#Sketches.config(:editor => "nano")

# Better Autocomplete
begin
  require 'bond'
  Bond.start
rescue LoadError
  puts $!
end

class Object
  # list methods which aren't in superclass
  def local_methods(obj = self)
    (obj.methods - Object.instance_methods).sort
  end
  
  # print documentation
  #
  #   ri 'Array#pop'
  #   Array.ri
  #   Array.ri :pop
  #   arr.ri :pop
  def ri(method = nil)
    unless method && method =~ /^[A-Z]/ # if class isn't specified
      klass = self.kind_of?(Class) ? name : self.class.name
      method = [klass, method].compact.join('#')
    end
    system 'ri', method.to_s
  end
end

# copy paste
def paste
  IO.popen('pbpaste') {|clipboard| clipboard.read}
end
def pbcopy(stuff)
  IO.popen('pbcopy', 'w+') {|clipboard| clipboard.write(stuff)}
end

# Work finders
def winny
  Student.find(151984)
end
def bradx3
  Parent.find_by_login("bradx3")
end
def bradteacher
  Teacher.find_by_login("bradteacher")
end
