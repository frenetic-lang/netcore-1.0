Vagrant.require_version ">= 1.6.0", "< 1.7.0"

Vagrant.configure("2") do |config|

  config.vm.box = "puppetlabs/ubuntu-12.04-64-puppet"

  config.vm.provider "virtualbox" do |v|
    v.memory = 2048
    v.cpus = 2
  end

  config.vm.provider "vmware_fusion" do |v|
    v.vmx["memsize"] = "2048"
    v.vmx["numvcpus"] = "2"
    v.gui = false
  end

  config.ssh.forward_x11 = true
  config.vm.provision "shell", path: "root-bootstrap.sh"
  config.vm.provision "shell", privileged: false, path: "user-bootstrap.sh"

  config.vm.synced_folder ".", "/home/vagrant/src"
  config.vm.hostname = "netcore1.0"

end