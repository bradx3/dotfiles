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

# Allow loading gems that aren't in bundler
def unbundled_require(gem)
  loaded = false
  if defined?(::Bundler)
    Gem.path.each do |gems_path|
      gem_path = Dir.glob("#{gems_path}/gems/#{gem}*").last
      unless gem_path.nil?
        $LOAD_PATH << "#{gem_path}/lib"
        require gem
        loaded = true
      end
    end
  else
    require gem
    loaded = true
  end
  raise(LoadError, "couldn't find #{gem}") unless loaded
  loaded
end
def load_gem(gem, &block)
  begin
    if unbundled_require gem
      yield if block_given?
    end
  rescue Exception => err
    warn "Couldn't load #{gem}: #{err}"
  end
end

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

# ips for benchmarking
load_gem("benchmark-ips")

# ap for nice printing
load_gem("awesome_print") do
  IRB::Irb.class_eval do
    def output_value
      ap(@context.last_value)
    end
  end
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
  alias_method :lm, :local_methods

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
def capture(&block)
  out = StringIO.new
  $stdout = out
  yield
  return out
ensure
  $stdout = STDOUT
end

def paste
  IO.popen('pbpaste') {|clipboard| clipboard.read}
end
def pbcopy(stuff)
  IO.popen('pbcopy', 'w+') {|clipboard| clipboard.write(stuff)}
end

# Work finders
def winny
  if Student.respond_to?(:fields)
    columns = Student.fields.map(&:first) # mongoid
  else
    columns = Student.column_names
  end

  if columns.include?("remote_id")
    Student.where(remote_id: 151984).first
  else
    Student.find(151984)
  end
end

def bradx3
  Parent.find_by_login("bradx3")
end
def bradteacher
  Teacher.find_by_login("bradteacher")
end
