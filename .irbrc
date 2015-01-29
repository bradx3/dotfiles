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
def unbundled_require(gem, options)
  loaded = false
  if defined?(::Bundler)
    Gem.path.each do |gems_path|
      gem_path = Dir.glob("#{gems_path}/gems/#{gem}*").last
      unless gem_path.nil?
        $LOAD_PATH << "#{gem_path}/lib"
        require(options[:require] || gem)
        loaded = true
      end
    end
  else
    require(options[:require] || gem)
    loaded = true
  end
  raise(LoadError, "couldn't find #{gem}") unless loaded
  loaded
end

def load_gem(gem, options = {}, &block)
  begin
    if unbundled_require(gem, options)
      yield if block_given?
    end
  rescue Exception => err
    warn "Couldn't load #{gem}: #{err}"
  end
end

# Table-ise results, etc
load_gem("hirb")

# "3".what?(3) lists all methods that return 3
load_gem("what_methods")

# ips for benchmarking
load_gem("benchmark-ips", require: "benchmark/ips")

# better autocomplete
load_gem("bond")

# ap for nice printing
load_gem("awesome_print") do
  IRB::Irb.class_eval do
    def output_value
      ap(@context.last_value)
    end
  end
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
def pi
  Practice.find(16)
end

def piu
  User.find(61642)
end

def bp
  Practice.find(662)
end

def bpu
  User.find(58099)
end

def first_with_scope(scope)
  if scope.count > 1
    puts("******************************")
    puts("#{scope.count} found, returning first")
    puts("******************************")
  end
  scope.first
end

def ube(email)
  # find user by email
  first_with_scope(User.where("lower(email) like ?", "%#{email.downcase}%"))
end

def pbn(name)
  # find practice by name
  first_with_scope(Practice.where("lower(name) like ?", "%#{name.downcase}%"))
end

def apbn(name)
  Practice.where("lower(name) like ?", "%#{name.downcase}%").to_a
end
