<?php
// +----------------------------------------------------------------------
// | Helper [PHP Helpler Library]
// +----------------------------------------------------------------------
// | Helper
// +----------------------------------------------------------------------
// | Licensed ( http://www.apache.org/licenses/LICENSE-2.0 )
// +----------------------------------------------------------------------
// | Author: axguowen <axguowen@qq.com>
// +----------------------------------------------------------------------

namespace axguowen\helper\contract;

interface Jsonable
{
    public function toJson($options = JSON_UNESCAPED_UNICODE);
}
