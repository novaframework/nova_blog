#{prefix => "",
  security => false,
  routes => [
             {"/", {nova_blog_main_controller, index}, #{}},
             {404, {nova_blog_main_controller, not_found}}
            ],
  statics => [
              {"/assets/[...]", "assets"}
             ]
 }.
