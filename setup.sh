sudo apt-get install nginx postgresql

sudo echo 'daemon off;' > /tmp/nginx.conf
sudo grep -v '^daemon' /etc/nginx/nginx.conf >> /tmp/nginx.conf
sudo mv /tmp/nginx.conf /etc/nginx/nginx.conf

cat > /tmp/nginx.conf <<EOF
description "Nginx web server"
start on runlevel [2345];
stop on [!2345];
respawn
exec /usr/sbin/nginx
EOF
sudo mv /tmp/nginx.conf /etc/init
sudo stop nginx
sudo start nginx

cat > /tmp/lamen-server.conf <<EOF
description "Lambda engine server"
start on runlevel [2345];
stop on [!2345];
respawn
script
HOME=/home/ubuntu exec /home/ubuntu/lambda-engine/lamen-server /home/ubuntu/lambda-engine/incoming /home/ubuntu/lambda-engine/apps /etc/nginx/sites-enabled/lambda-engine /etc/init
end script
EOF
chmod +x /home/ubuntu/lambda-engine/lamen-server
sudo mv /tmp/lamen-server.conf /etc/init

sudo mv -i /home/ubuntu/lamen-server /home/ubuntu/lambda-dengine/lamen-server
sudo stop lamen-server
sudo start lamen-server

mkdir -p /home/ubuntu/lambda-engine/incoming
mkdir -p /home/ubuntu/lambda-engine/apps
