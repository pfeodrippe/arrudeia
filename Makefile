clj_cmd = env clj -O:default-options

.PHONY: build
build:
	mkdir -p target
	$(clj_cmd) -A:depstar -m hf.depstar.uberjar target/arrudeia.jar

.PHONY: deploy
deploy:
	mvn deploy:deploy-file -Dfile=target/arrudeia.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/

.PHONY: test
test:
	$(clj_cmd) -A:test

.PHONY: autotest
autotest:
	$(clj_cmd) -A:test --watch
