#{prefix => "",
  security => false,
  routes => [
             {"/", {nova_blog_main_controller, index}},
             {"/blog", {nova_blog_blog_controller, index}}
            ],
  statics => [
              {"/assets/[...]", "assets"}
             ]
 }.
