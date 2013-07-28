module MpiExec
  def self.mpiexec
    @mpiexec ||= ENV["mpiexec"] || "default"
  end
  def self.mpiexec=(val)
    @mpiexec = val
  end

  def run(*args)
    # Sanitize options
    options = args.last.is_a?(Hash) ? args.pop : {}

    options[:cores] ||= 1
    options[:cores] = Integer(options[:cores])

    args << options

    # Get appropriate mpiexec command
    case MpiExec.mpiexec
      when "aprun"
        shell_command = aprun(*args)
      else
        shell_command = mpiexec(*args)
    end

    # Execute command
    puts "#{shell_command}"
    puts `#{shell_command}`
  end

  def aprun(program, options)
    hector_max_cores_per_node = 32

    cores = options[:cores]

    cores_per_node = options[:cores_per_node] || options[:cores]
    cores_per_node = [ hector_max_cores_per_node, options[:cores] ].min

    "time aprun -n #{cores} -N #{cores_per_node} #{program}"
  end

  def mpiexec(program, options)
    "time mpiexec -n #{options[:cores]} #{program}"
  end
end
