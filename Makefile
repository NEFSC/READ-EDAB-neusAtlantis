IMAGE=atlantis_docker
USER=$(id -u)
GROUP=$(id -g)

run: build
	 echo "PWD is :$(PWD)"
	 docker run -v $(PWD)/currentVersion:/app/model --user $(USER):$(GROUP) atlantis_docker
#	 docker run -v $(PWD)/example:/app/model atlantis_docker

build:
	docker build --build-arg REGISTRY=docker-registry.it.csiro.au -t $(IMAGE) .

clean:
	docker system prune -f