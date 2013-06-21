module MpiExec
  def self.mpienv
    @mpienv ||= ENV["mpienv"] || "default"
  end
  def self.mpienv=(val)
    @mpienv = val
  end

  def run(*args)
    # Sanitize options
    options = args.last.is_a?(Hash) ? args.last : {}

    options[:cores] ||= 1

    # Get appropriate mpiexec command
    case MpiExec.mpienv
      when "cray"
        shell_command = aprun(*args)
      else
        shell_command = mpiexec(*args)
    end

    # Execute command
    puts "#{shell_command}"
    puts `#{shell_command}`
  end

  def aprun(program, options)
    cores = options[:cores]

    cores_per_node = options[:cores_per_node] || options[:cores]
    cores_per_node = [ 24, options[:cores] ].min

    "aprun -n #{cores} -N #{cores_per_node} #{program}"
  end

  def mpiexec(program, options)
    "mpiexec -n #{options[:cores]} #{program}"
  end
end
