watch:
	clj -X harooosh.site/generate-site-cmd ;\
	shadow-cljs watch :app

build-release:
	rm -rf public ;\
	rm -rf public/js/compiled ;\
  clj -X harooosh.site/generate-site-cmd ;\
	shadow-cljs release :app ;\
	rm -rf docs ;\
	cp -R public docs ;\
	cp CNAME docs/CNAME

build-site:
	clj -X harooosh.site/generate-site-cmd
