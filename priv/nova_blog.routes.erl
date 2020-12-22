#{prefix => "",
  security => false,
  routes => [
             {"/", {nova_blog_main_controller, index}, #{}}
            ],
  statics => [
              {"/assets/[...]", "assets"}
             ]
 }.
