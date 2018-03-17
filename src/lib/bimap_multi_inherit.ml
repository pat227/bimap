(*do this twice; once with objects/classes and once with modules
  --m1 and m2 are empty maps that need to be provided by client code-- *)
module Bimap_multi_inherit = struct
  class ['a,'b] bimap_multi_class m1 m2 = object(self)
    inherit Bimap.bimap_class as super
  end
end 
