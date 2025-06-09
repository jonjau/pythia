Vagrant.configure("2") do |config|
    # Use an Ubuntu base box
    config.vm.box = "ubuntu/bionic64"  # or another of your choice
  
    # Enable Docker provisioning
    config.vm.provision "docker"
  
    # Forward any ports if needed
    config.vm.network "forwarded_port", guest: 3000, host: 3000
  
    # Share the directory containing the Dockerfile with the VM
    config.vm.synced_folder ".", "/vagrant"
  
    # Run Docker commands after Docker is installed
    config.vm.provision "docker" do |d|
      d.build_image "/vagrant", args: "-t pythia"
      d.run "pythia", args: "-p 3000:3000 -d"
    end
  
    config.vm.provider "virtualbox" do |vb|
      vb.memory = "4096"
      vb.cpus = 2
    end
  end