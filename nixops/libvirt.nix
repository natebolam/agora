{
  agora = {
    deployment.targetEnv = "libvirtd";
    deployment.libvirtd = {
      headless = true;
      memorySize = 2048;
      vcpu = 4;
    };

    # Serial console over `virsh`
    boot.kernelParams = [ "console=ttyS0,115200" ];
    deployment.libvirtd.extraDevicesXML = ''
      <serial type='pty'>
      <target port='0'/>
      </serial>
      <console type='pty'>
      <target type='serial' port='0'/>
      </console>
    '';
  };
}
