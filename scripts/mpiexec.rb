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
    options[:mpi_procs] ||= options[:cores]
    options[:mpi_procs] = Integer(options[:mpi_procs])
    options[:openmp_per_mpi] ||= 1
    options[:openmp_per_mpi] = Integer(options[:openmp_per_mpi])

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
    mpi_procs = options[:mpi_procs]
    openmp_per_mpi = options[:openmp_per_mpi]

    procs_per_node = options[:procs_per_node] || mpi_procs
    if procs_per_node*openmp_per_mpi > hector_max_cores_per_node
        procs_per_node = (hector_max_cores_per_node/openmp_per_mpi).floor
    end

    "OMP_NUM_THREADS=#{openmp_per_mpi}
        time aprun -n #{mpi_procs} -N #{procs_per_node} -d #{openmp_per_mpi} \
        #{program}"
  end

  def mpiexec(program, options)
    "OMP_NUM_THREADS=#{options[:openmp_per_mpi]} \
        time mpiexec -n #{options[:mpi_procs]} #{program}"
  end
end
