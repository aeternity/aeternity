SWAGGER_UI_GIT =https://github.com/swagger-api/swagger-ui.git
SWAGGER_UI_GIT_DIR =./priv/swagger-ui
SWAGGER_DOCS_DIR =./priv/swagger-docs

.PHONY: swagger-docs
swagger-docs: |$(SWAGGER_UI_GIT_DIR)/.git
	@mkdir -p $(SWAGGER_DOCS_DIR)
	@cp -p $(SWAGGER_UI_GIT_DIR)/dist/* $(SWAGGER_DOCS_DIR)
	@cp -p ./priv/swagger.json $(SWAGGER_DOCS_DIR)
	@sed -ibkp 's/http:\/\/petstore.swagger.io\/v2\/swagger.json/swagger.json/g' $(SWAGGER_DOCS_DIR)/index.html

$(SWAGGER_UI_GIT_DIR)/.git:
	@git clone -n --depth 1 $(SWAGGER_UI_GIT) $(SWAGGER_UI_GIT_DIR)
	( cd $(SWAGGER_UI_GIT_DIR) && git checkout master dist ; )

.PHONY: clean
clean:
	rm -rf $(SWAGGER_DOCS_DIR)

.PHONY: distclean
distclean:
	rm -rf $(SWAGGER_DOCS_DIR) $(SWAGGER_UI_GIT_DIR)

updateswagger:
	@if [ -f $(SWAGGER_DOCS_DIR)/swagger.json ]; then \
		cp -p ./priv/swagger.json $(SWAGGER_DOCS_DIR); \
	fi
