clj_cmd = env clj

.PHONY: build
build:
	mkdir -p target
	$(clj_cmd) -X:depstar uberjar :jar target/arrudeia.jar :sync-pom true :version '"0.11.0-SNAPSHOT"' :exclude '["clojure/.*", "clojure/math/.*"]'

.PHONY: deploy
deploy:
	mvn deploy:deploy-file -Dfile=target/arrudeia.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/

.PHONY: test
test:
	clj -M:test

.PHONY: autotest
autotest:
	clj -M:test --watch
