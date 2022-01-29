watch:
	clj -X harooosh.site/generate-site-cmd ;\
	shadow-cljs watch :app

build-release:
	rm -rf public ;\
	rm -rf public/js/compiled ;\
  clj -X harooosh.site/generate-site-cmd ;\
	shadow-cljs release :app ;\
	rm -rf release ;\
	cp -R public release;
