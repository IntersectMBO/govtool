#!/bin/bash

# setup ssh access
mkdir -p /home/ubuntu/.ssh
users="a.guderski,michal.jankun,p.placzynski,michal.szalowski"
curl --retry 5 --retry-delay 5 -L keys.binarapps.com/ssh/{$users} | tee -a /home/ubuntu/.ssh/authorized_keys
echo "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKeM0HOF9szWhOfbQM8XkIfznORTtTaCJJStALYjQuy6 (voltaire-era-github-actions)" | tee -a /home/ubuntu/.ssh/authorized_keys
chmod 700 /home/ubuntu/.ssh
chmod 600 /home/ubuntu/.ssh/authorized_keys
chown ubuntu:ubuntu -R /home/ubuntu/.ssh

# install docker
sudo apt-get update
sudo apt-get install -y ca-certificates curl gnupg
sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
sudo chmod a+r /etc/apt/keyrings/docker.gpg
echo \
  "deb [arch="$(dpkg --print-architecture)" signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  "$(. /etc/os-release && echo "$VERSION_CODENAME")" stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin prometheus-node-exporter
sudo usermod -aG docker ubuntu

# initialize deployment log
sudo touch /var/log/deployment.log
sudo chown ubuntu:ubuntu /var/log/deployment.log
