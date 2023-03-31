dir.create("/contrib/Joseph.Caracappa/out1/")
#x= 'sudo podman run --rm  -d --name scenario1 --mount "type=bind,src=/usr/local/neus-atlantis/currentVersion/,dst=/app/model" --mount "type=bind,src=/contrib/Joseph.Caracappa/out1,dst=/app/model/output/" atlantis:6536'
#system(x)
write.table(x,file = '/contrib/Joseph.Caracappa/out1/test.txt')
