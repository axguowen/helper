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

namespace axguowen\helper;

/**
 * 数组小助手基础类
 */
if (class_exists('think\helper\Str')) {
    class BaseStr extends \think\helper\Str
    {}
} else {
    class BaseStr
    {
        protected static $snakeCache = [];

        protected static $camelCache = [];

        protected static $studlyCache = [];

        /**
         * 检查字符串中是否包含某些字符串
         * @param string       $haystack
         * @param string|array $needles
         * @return bool
         */
        public static function contains(string $haystack, $needles): bool
        {
            foreach ((array) $needles as $needle) {
                if ('' != $needle && mb_strpos($haystack, $needle) !== false) {
                    return true;
                }
            }

            return false;
        }

        /**
         * 检查字符串是否以某些字符串结尾
         *
         * @param  string       $haystack
         * @param  string|array $needles
         * @return bool
         */
        public static function endsWith(string $haystack, $needles): bool
        {
            foreach ((array) $needles as $needle) {
                if ((string) $needle === static::substr($haystack, -static::length($needle))) {
                    return true;
                }
            }

            return false;
        }

        /**
         * 检查字符串是否以某些字符串开头
         *
         * @param  string       $haystack
         * @param  string|array $needles
         * @return bool
         */
        public static function startsWith(string $haystack, $needles): bool
        {
            foreach ((array) $needles as $needle) {
                if ('' != $needle && mb_strpos($haystack, $needle) === 0) {
                    return true;
                }
            }

            return false;
        }

        /**
         * 获取指定长度的随机字母数字组合的字符串
         *
         * @param  int $length
         * @param  int $type
         * @param  string $addChars
         * @return string
         */
        public static function random(int $length = 6, int $type = null, string $addChars = ''): string
        {
            $str = '';
            switch ($type) {
                case 0:
                    $chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' . $addChars;
                    break;
                case 1:
                    $chars = str_repeat('0123456789', 3);
                    break;
                case 2:
                    $chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' . $addChars;
                    break;
                case 3:
                    $chars = 'abcdefghijklmnopqrstuvwxyz' . $addChars;
                    break;
                case 4:
                    $chars = "们以我到他会作时要动国产的一是工就年阶义发成部民可出能方进在了不和有大这主中人上为来分生对于学下级地个用同行面说种过命度革而多子后自社加小机也经力线本电高量长党得实家定深法表着水理化争现所二起政三好十战无农使性前等反体合斗路图把结第里正新开论之物从当两些还天资事队批点育重其思与间内去因件日利相由压员气业代全组数果期导平各基或月毛然如应形想制心样干都向变关问比展那它最及外没看治提五解系林者米群头意只明四道马认次文通但条较克又公孔领军流入接席位情运器并飞原油放立题质指建区验活众很教决特此常石强极土少已根共直团统式转别造切九你取西持总料连任志观调七么山程百报更见必真保热委手改管处己将修支识病象几先老光专什六型具示复安带每东增则完风回南广劳轮科北打积车计给节做务被整联步类集号列温装即毫知轴研单色坚据速防史拉世设达尔场织历花受求传口断况采精金界品判参层止边清至万确究书" . $addChars;
                    break;
                default:
                    $chars = 'ABCDEFGHIJKMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz23456789' . $addChars;
                    break;
            }
            if ($length > 10) {
                $chars = $type == 1 ? str_repeat($chars, $length) : str_repeat($chars, 5);
            }
            if ($type != 4) {
                $chars = str_shuffle($chars);
                $str = substr($chars, 0, $length);
            } else {
                for ($i = 0; $i < $length; $i++) {
                    $str .= mb_substr($chars, floor(mt_rand(0, mb_strlen($chars, 'utf-8') - 1)), 1);
                }
            }
            return $str;
        }

        /**
         * 字符串转小写
         *
         * @param  string $value
         * @return string
         */
        public static function lower(string $value): string
        {
            return mb_strtolower($value, 'UTF-8');
        }

        /**
         * 字符串转大写
         *
         * @param  string $value
         * @return string
         */
        public static function upper(string $value): string
        {
            return mb_strtoupper($value, 'UTF-8');
        }

        /**
         * 获取字符串的长度
         *
         * @param  string $value
         * @return int
         */
        public static function length(string $value): int
        {
            return mb_strlen($value);
        }

        /**
         * 截取字符串
         *
         * @param  string   $string
         * @param  int      $start
         * @param  int|null $length
         * @return string
         */
        public static function substr(string $string, int $start, int $length = null): string
        {
            return mb_substr($string, $start, $length, 'UTF-8');
        }

        /**
         * 驼峰转下划线
         *
         * @param  string $value
         * @param  string $delimiter
         * @return string
         */
        public static function snake(string $value, string $delimiter = '_'): string
        {
            $key = $value;

            if (isset(static::$snakeCache[$key][$delimiter])) {
                return static::$snakeCache[$key][$delimiter];
            }

            if (!ctype_lower($value)) {
                $value = preg_replace('/\s+/u', '', ucwords($value));

                $value = static::lower(preg_replace('/(.)(?=[A-Z])/u', '$1' . $delimiter, $value));
            }

            return static::$snakeCache[$key][$delimiter] = $value;
        }

        /**
         * 下划线转驼峰(首字母小写)
         *
         * @param  string $value
         * @return string
         */
        public static function camel(string $value): string
        {
            if (isset(static::$camelCache[$value])) {
                return static::$camelCache[$value];
            }

            return static::$camelCache[$value] = lcfirst(static::studly($value));
        }

        /**
         * 下划线转驼峰(首字母大写)
         *
         * @param  string $value
         * @return string
         */
        public static function studly(string $value): string
        {
            $key = $value;

            if (isset(static::$studlyCache[$key])) {
                return static::$studlyCache[$key];
            }

            $value = ucwords(str_replace(['-', '_'], ' ', $value));

            return static::$studlyCache[$key] = str_replace(' ', '', $value);
        }

        /**
         * 转为首字母大写的标题格式
         *
         * @param  string $value
         * @return string
         */
        public static function title(string $value): string
        {
            return mb_convert_case($value, MB_CASE_TITLE, 'UTF-8');
        }
    }
}

/**
 * 字符串小助手
 */
class Str extends BaseStr
{
    /**
     * 获取类名(不包含命名空间)
     *
     * @param mixed $class 类名
     * @return string
     */
    public static function classBaseName($class)
    {
        $class = is_object($class) ? get_class($class) : $class;
        return basename(str_replace('\\', '/', $class));
    }

    /**
     * 获取类的命名空间
     *
     * @param mixed $class 类名
     * @return string
     */
    public static function classSpaceName($class)
    {
        $class = is_object($class) ? get_class($class) : $class;
        // 获取不包含命名空间的类名
        $classBaseName = self::classBaseName($class);
        return str_replace($classBaseName, '', $class);
    }

    /**
     * 将路径转换为类名
     *
     * @param mixed $path 路径
     * @return string
     */
    public static function pathToClassName($path)
    {
        // 获取路径
        $spaceName = self::classSpaceName($path);
        // 获取不带命名空间的类名
        $baseName = self::classBaseName($path);
        // 转换为驼峰
        $baseName = self::studly($baseName);
        // 返回
        return $spaceName . $baseName;
    }

    /**
     * 字符串命名风格转换
     * type 0 将Java风格转换为C的风格 1 将C风格转换为Java的风格
     * @param string $name    字符串
     * @param int    $type    转换类型
     * @param bool   $ucfirst 首字母是否大写（驼峰规则）
     * @return string
     */
    public static function parseName($name, $type = 0, $ucfirst = true)
    {
        if ($type) {
            $name = preg_replace_callback('/_([a-zA-Z])/', function ($match) {
                return strtoupper($match[1]);
            }, $name);

            return $ucfirst ? ucfirst($name) : lcfirst($name);
        }

        return strtolower(trim(preg_replace('/[A-Z]/', '_\\0', $name), '_'));
    }

    /**
     * 数字前面补0
     * @param string $num       要补0的数字
     * @param int    $length    总长度
     * @return string
     */
    public static function digit($num, $length = 2)
    {
        return str_pad($num, $length, '0', STR_PAD_LEFT);
    }

    /**
     * 获取字符串在另一个字符串中第n次出现的位置
     * @param string    $str        字符串
     * @param string    $find       要查找的字符串
     * @param string    $start      起始位置
     * @param string    $n          第几次出现
     * @return string
     */
    public function strposNth($str, $find, $start = 0, $n = 1)
    {
        // 位置
        $pos_val = 0;
        // 遍历
        for ($i=0;$i<$n;$i++){
            // 不是第一次循环则不用考虑开始位置
            if($i > 0){
                $start = 0;
            }
            $pos = strpos($str, $find, $start);
            // 如果不存在
            if($pos == false){
                $pos_val = false;
                break;
            }
            // 重新构造字符串
            $str = substr($str, $pos + 1);
            // 获取位置
            $pos_val = $pos_val + $pos + 1;
        }
        if($pos_val === false){
            return false;
        }
        return $pos_val - 1;
    }

    /**
     * 目录格式转换
     * @param   string $path        目录
     * @param   bool   $deprPre     是否需要开头分隔符
     * @param   bool   $deprEnd     是否需要结尾分隔符
     * @return  string
     */
    public static function parsePath($path = '', $deprPre = false, $deprEnd = false)
    {
        // 去掉重复目录分隔符
        $path = preg_replace('/\/{1,}/', '/', $path);
        // 需要开头分隔符
        if($deprPre){
            // 没有匹配到开头分隔符则补齐开头分隔符
            if(!preg_match('/^\//', $path)){
                $path = '/' . $path;
            }
        }
        // 不需要开头分隔符
        else{
            // 匹配到开头分隔符则删掉开头分隔符
            if(preg_match('/^\//', $path)){
                $path = preg_replace('/^\//', '', $path);
            }
        }
        // 需要结尾分隔符
        if($deprEnd){
            // 补齐结束分隔符
            if(!preg_match('/\/$/',$path)){
                $path = $path . '/';
            }
        }
        // 不需要结尾分隔符
        else{
            // 匹配到开头分隔符则删掉开头分隔符
            if(preg_match('/\/$/',$path)){
                $path = preg_replace('/\/$/', '', $path);
            }
        }
        // 返回
        return $path;
    }

    /**
     * 解析规则
     * @param   string      $rule   规则
     * @param   array       $params 参数
     * @return  string
     */
    public static function parseRule($rule = '', $params = [])
    {
        // 如果规则为空
        if(empty($rule)) {
            return '';
		}
        // 如果参数为空
        if(empty($params)){
            return $rule;
        }
        // 遍历参数并替换
		foreach ($params as $key => $val) {
			if ('' !== trim($val)) {
				$rule = str_replace('{'.$key.'}', $val, $rule);
			}
		}
        // 返回
        return $rule;
    }

    /**
     * 获取token加密串
     * @param string $token 令牌
     * @return string
     */
    public static function getTokenHash($token = 'null')
    {
        $value = 'itzjj_authorization_' . $token . '_v20221111';
        return hash('md5', $value);
    }

    /**
     * 获取简体字符串
     * @return string
     */
    public static function simplifiedChar(){
		return '锕皑蔼碍爱嗳嫒瑷暧霭谙铵鹌肮袄奥媪骜鳌坝罢钯摆败呗颁办绊钣帮绑镑谤剥饱宝报鲍鸨龅辈贝钡狈备惫鹎贲锛绷笔毕毙币闭荜哔滗铋筚跸边编贬变辩辫苄缏笾标骠飑飙镖镳鳔鳖别瘪濒滨宾摈傧缤槟殡膑镔髌鬓饼禀拨钵铂驳饽钹鹁补钸财参蚕残惭惨灿骖黪苍舱仓沧厕侧册测恻层诧锸侪钗搀掺蝉馋谗缠铲产阐颤冁谄谶蒇忏婵骣觇禅镡场尝长偿肠厂畅伥苌怅阊鲳钞车彻砗尘陈衬伧谌榇碜龀撑称惩诚骋枨柽铖铛痴迟驰耻齿炽饬鸱冲冲虫宠铳畴踌筹绸俦帱雠橱厨锄雏础储触处刍绌蹰传钏疮闯创怆锤缍纯鹑绰辍龊辞词赐鹚聪葱囱从丛苁骢枞凑辏蹿窜撺错锉鹾达哒鞑带贷骀绐担单郸掸胆惮诞弹殚赕瘅箪当挡党荡档谠砀裆捣岛祷导盗焘灯邓镫敌涤递缔籴诋谛绨觌镝颠点垫电巅钿癫钓调铫鲷谍叠鲽钉顶锭订铤丢铥东动栋冻岽鸫窦犊独读赌镀渎椟牍笃黩锻断缎簖兑队对怼镦吨顿钝炖趸夺堕铎鹅额讹恶饿谔垩阏轭锇锷鹗颚颛鳄诶儿尔饵贰迩铒鸸鲕发罚阀珐矾钒烦贩饭访纺钫鲂飞诽废费绯镄鲱纷坟奋愤粪偾丰枫锋风疯冯缝讽凤沣肤辐抚辅赋复负讣妇缚凫驸绂绋赙麸鲋鳆钆该钙盖赅杆赶秆赣尴擀绀冈刚钢纲岗戆镐睾诰缟锆搁鸽阁铬个纥镉颍给亘赓绠鲠龚宫巩贡钩沟苟构购够诟缑觏蛊顾诂毂钴锢鸪鹄鹘剐挂鸹掴关观馆惯贯诖掼鹳鳏广犷规归龟闺轨诡贵刽匦刿妫桧鲑鳜辊滚衮绲鲧锅国过埚呙帼椁蝈铪骇韩汉阚绗颉号灏颢阂鹤贺诃阖蛎横轰鸿红黉讧荭闳鲎壶护沪户浒鹕哗华画划话骅桦铧怀坏欢环还缓换唤痪焕涣奂缳锾鲩黄谎鳇挥辉毁贿秽会烩汇讳诲绘诙荟哕浍缋珲晖荤浑诨馄阍获货祸钬镬击机积饥迹讥鸡绩缉极辑级挤几蓟剂济计记际继纪讦诘荠叽哜骥玑觊齑矶羁虿跻霁鲚鲫夹荚颊贾钾价驾郏浃铗镓蛲歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧谏缣戋戬睑鹣笕鲣鞯将浆蒋桨奖讲酱绛缰胶浇骄娇搅铰矫侥脚饺缴绞轿较挢峤鹪鲛阶节洁结诫届疖颌鲒紧锦仅谨进晋烬尽劲荆茎卺荩馑缙赆觐鲸惊经颈静镜径痉竞净刭泾迳弪胫靓纠厩旧阄鸠鹫驹举据锯惧剧讵屦榉飓钜锔窭龃鹃绢锩镌隽觉决绝谲珏钧军骏皲开凯剀垲忾恺铠锴龛闶钪铐颗壳课骒缂轲钶锞颔垦恳龈铿抠库裤喾块侩郐哙脍宽狯髋矿旷况诓诳邝圹纩贶亏岿窥馈溃匮蒉愦聩篑阃锟鲲扩阔蛴蜡腊莱来赖崃徕涞濑赉睐铼癞籁蓝栏拦篮阑兰澜谰揽览懒缆烂滥岚榄斓镧褴琅阆锒捞劳涝唠崂铑铹痨乐鳓镭垒类泪诔缧篱狸离鲤礼丽厉励砾历沥隶俪郦坜苈莅蓠呖逦骊缡枥栎轹砺锂鹂疠粝跞雳鲡鳢俩联莲连镰怜涟帘敛脸链恋炼练蔹奁潋琏殓裢裣鲢粮凉两辆谅魉疗辽镣缭钌鹩猎临邻鳞凛赁蔺廪檩辚躏龄铃灵岭领绫棂蛏鲮馏刘浏骝绺镏鹨龙聋咙笼垄拢陇茏泷珑栊胧砻楼娄搂篓偻蒌喽嵝镂瘘耧蝼髅芦卢颅庐炉掳卤虏鲁赂禄录陆垆撸噜闾泸渌栌橹轳辂辘氇胪鸬鹭舻鲈峦挛孪滦乱脔娈栾鸾銮抡轮伦仑沦纶论囵萝罗逻锣箩骡骆络荦猡泺椤脶镙驴吕铝侣屡缕虑滤绿榈褛锊呒妈玛码蚂马骂吗唛嬷杩买麦卖迈脉劢瞒馒蛮满谩缦镘颡鳗猫锚铆贸麽没镁门闷们扪焖懑钔锰梦眯谜弥觅幂芈谧猕祢绵缅渑腼黾庙缈缪灭悯闽闵缗鸣铭谬谟蓦馍殁镆谋亩钼呐钠纳难挠脑恼闹铙讷馁内拟腻铌鲵撵辇鲶酿鸟茑袅聂啮镊镍陧蘖嗫颟蹑柠狞宁拧泞苎咛聍钮纽脓浓农侬哝驽钕诺傩疟欧鸥殴呕沤讴怄瓯盘蹒庞抛疱赔辔喷鹏纰罴铍骗谝骈飘缥频贫嫔苹凭评泼颇钋扑铺朴谱镤镨栖脐齐骑岂启气弃讫蕲骐绮桤碛颀颃鳍牵钎铅迁签谦钱钳潜浅谴堑佥荨悭骞缱椠钤枪呛墙蔷强抢嫱樯戗炝锖锵镪羟跄锹桥乔侨翘窍诮谯荞缲硗跷窃惬锲箧钦亲寝锓轻氢倾顷请庆揿鲭琼穷茕蛱巯赇虮鳅趋区躯驱龋诎岖阒觑鸲颧权劝诠绻辁铨却鹊确阕阙悫让饶扰绕荛娆桡热韧认纫饪轫荣绒嵘蝾缛铷颦软锐蚬闰润洒萨飒鳃赛伞毵糁丧骚扫缫涩啬铯穑杀刹纱铩鲨筛晒酾删闪陕赡缮讪姗骟钐鳝墒伤赏垧殇觞烧绍赊摄慑设厍滠畲绅审婶肾渗诜谂渖声绳胜师狮湿诗时蚀实识驶势适释饰视试谥埘莳弑轼贳铈鲥寿兽绶枢输书赎属术树竖数摅纾帅闩双谁税顺说硕烁铄丝饲厮驷缌锶鸶耸怂颂讼诵擞薮馊飕锼苏诉肃谡稣虽随绥岁谇孙损笋荪狲缩琐锁唢睃獭挞闼铊鳎台态钛鲐摊贪瘫滩坛谭谈叹昙钽锬顸汤烫傥饧铴镗涛绦讨韬铽腾誊锑题体屉缇鹈阗条粜龆鲦贴铁厅听烃铜统恸头钭秃图钍团抟颓蜕饨脱鸵驮驼椭箨鼍袜娲腽弯湾顽万纨绾网辋韦违围为潍维苇伟伪纬谓卫诿帏闱沩涠玮韪炜鲔温闻纹稳问阌瓮挝蜗涡窝卧莴龌呜钨乌诬无芜吴坞雾务误邬庑怃妩骛鹉鹜锡牺袭习铣戏细饩阋玺觋虾辖峡侠狭厦吓硖鲜纤贤衔闲显险现献县馅羡宪线苋莶藓岘猃娴鹇痫蚝籼跹厢镶乡详响项芗饷骧缃飨萧嚣销晓啸哓潇骁绡枭箫协挟携胁谐写泻谢亵撷绁缬锌衅兴陉荥凶汹锈绣馐鸺虚嘘须许叙绪续诩顼轩悬选癣绚谖铉镟学谑泶鳕勋询寻驯训讯逊埙浔鲟压鸦鸭哑亚讶垭娅桠氩阉烟盐严岩颜阎艳厌砚彦谚验厣赝俨兖谳恹闫酽魇餍鼹鸯杨扬疡阳痒养样炀瑶摇尧遥窑谣药轺鹞鳐爷页业叶靥谒邺晔烨医铱颐遗仪蚁艺亿忆义诣议谊译异绎诒呓峄饴怿驿缢轶贻钇镒镱瘗舣荫阴银饮隐铟瘾樱婴鹰应缨莹萤营荧蝇赢颖茔莺萦蓥撄嘤滢潆璎鹦瘿颏罂哟拥佣痈踊咏镛优忧邮铀犹诱莸铕鱿舆鱼渔娱与屿语狱誉预驭伛俣谀谕蓣嵛饫阈妪纡觎欤钰鹆鹬龉鸳渊辕园员圆缘远橼鸢鼋约跃钥粤悦阅钺郧匀陨运蕴酝晕韵郓芸恽愠纭韫殒氲杂灾载攒暂赞瓒趱錾赃脏驵凿枣责择则泽赜啧帻箦贼谮赠综缯轧铡闸栅诈斋债毡盏斩辗崭栈战绽谵张涨帐账胀赵诏钊蛰辙锗这谪辄鹧贞针侦诊镇阵浈缜桢轸赈祯鸩挣睁狰争帧症郑证诤峥钲铮筝织职执纸挚掷帜质滞骘栉栀轵轾贽鸷蛳絷踬踯觯钟终种肿众锺诌轴皱昼骤纣绉猪诸诛烛瞩嘱贮铸驻伫槠铢专砖转赚啭馔颞桩庄装妆壮状锥赘坠缀骓缒谆准着浊诼镯兹资渍谘缁辎赀眦锱龇鲻踪总纵偬邹诹驺鲰诅组镞钻缵躜鳟翱并卜沉丑淀迭斗范干皋硅柜后伙秸杰诀夸里凌么霉捻凄扦圣尸抬涂洼喂污锨咸蝎彝涌游吁御愿岳云灶扎札筑于志注凋讠谫郄勐凼坂垅垴埯埝苘荬荮莜莼菰藁揸吒吣咔咝咴噘噼嚯幞岙嵴彷徼犸狍馀馇馓馕愣憷懔丬溆滟溷漤潴澹甯纟绔绱珉枧桊桉槔橥轱轷赍肷胨飚煳煅熘愍淼砜磙眍钚钷铘铞锃锍锎锏锘锝锪锫锿镅镎镢镥镩镲稆鹋鹛鹱疬疴痖癯裥襁耢颥螨麴鲅鲆鲇鲞鲴鲺鲼鳊鳋鳘鳙鞒鞴齄';
	}

    /**
     * 获取繁体字符串
     * @return string
     */
    public static function traditionalChar(){
        return '錒皚藹礙愛噯嬡璦曖靄諳銨鵪骯襖奧媼驁鰲壩罷鈀擺敗唄頒辦絆鈑幫綁鎊謗剝飽寶報鮑鴇齙輩貝鋇狽備憊鵯賁錛繃筆畢斃幣閉蓽嗶潷鉍篳蹕邊編貶變辯辮芐緶籩標驃颮飆鏢鑣鰾鱉別癟瀕濱賓擯儐繽檳殯臏鑌髕鬢餅稟撥缽鉑駁餑鈸鵓補鈽財參蠶殘慚慘燦驂黲蒼艙倉滄廁側冊測惻層詫鍤儕釵攙摻蟬饞讒纏鏟產闡顫囅諂讖蕆懺嬋驏覘禪鐔場嘗長償腸廠暢倀萇悵閶鯧鈔車徹硨塵陳襯傖諶櫬磣齔撐稱懲誠騁棖檉鋮鐺癡遲馳恥齒熾飭鴟沖衝蟲寵銃疇躊籌綢儔幬讎櫥廚鋤雛礎儲觸處芻絀躕傳釧瘡闖創愴錘綞純鶉綽輟齪辭詞賜鶿聰蔥囪從叢蓯驄樅湊輳躥竄攛錯銼鹺達噠韃帶貸駘紿擔單鄲撣膽憚誕彈殫賧癉簞當擋黨蕩檔讜碭襠搗島禱導盜燾燈鄧鐙敵滌遞締糴詆諦綈覿鏑顛點墊電巔鈿癲釣調銚鯛諜疊鰈釘頂錠訂鋌丟銩東動棟凍崠鶇竇犢獨讀賭鍍瀆櫝牘篤黷鍛斷緞籪兌隊對懟鐓噸頓鈍燉躉奪墮鐸鵝額訛惡餓諤堊閼軛鋨鍔鶚顎顓鱷誒兒爾餌貳邇鉺鴯鮞發罰閥琺礬釩煩販飯訪紡鈁魴飛誹廢費緋鐨鯡紛墳奮憤糞僨豐楓鋒風瘋馮縫諷鳳灃膚輻撫輔賦復負訃婦縛鳧駙紱紼賻麩鮒鰒釓該鈣蓋賅桿趕稈贛尷搟紺岡剛鋼綱崗戇鎬睪誥縞鋯擱鴿閣鉻個紇鎘潁給亙賡綆鯁龔宮鞏貢鉤溝茍構購夠詬緱覯蠱顧詁轂鈷錮鴣鵠鶻剮掛鴰摑關觀館慣貫詿摜鸛鰥廣獷規歸龜閨軌詭貴劊匭劌媯檜鮭鱖輥滾袞緄鯀鍋國過堝咼幗槨蟈鉿駭韓漢闞絎頡號灝顥閡鶴賀訶闔蠣橫轟鴻紅黌訌葒閎鱟壺護滬戶滸鶘嘩華畫劃話驊樺鏵懷壞歡環還緩換喚瘓煥渙奐繯鍰鯇黃謊鰉揮輝毀賄穢會燴匯諱誨繪詼薈噦澮繢琿暉葷渾諢餛閽獲貨禍鈥鑊擊機積饑跡譏雞績緝極輯級擠幾薊劑濟計記際繼紀訐詰薺嘰嚌驥璣覬齏磯羈蠆躋霽鱭鯽夾莢頰賈鉀價駕郟浹鋏鎵蟯殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗諫縑戔戩瞼鶼筧鰹韉將漿蔣槳獎講醬絳韁膠澆驕嬌攪鉸矯僥腳餃繳絞轎較撟嶠鷦鮫階節潔結誡屆癤頜鮚緊錦僅謹進晉燼盡勁荊莖巹藎饉縉贐覲鯨驚經頸靜鏡徑痙競凈剄涇逕弳脛靚糾廄舊鬮鳩鷲駒舉據鋸懼劇詎屨櫸颶鉅鋦窶齟鵑絹錈鐫雋覺決絕譎玨鈞軍駿皸開凱剴塏愾愷鎧鍇龕閌鈧銬顆殼課騍緙軻鈳錁頷墾懇齦鏗摳庫褲嚳塊儈鄶噲膾寬獪髖礦曠況誆誑鄺壙纊貺虧巋窺饋潰匱蕢憒聵簣閫錕鯤擴闊蠐蠟臘萊來賴崍徠淶瀨賚睞錸癩籟藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫嵐欖斕鑭襤瑯閬鋃撈勞澇嘮嶗銠鐒癆樂鰳鐳壘類淚誄縲籬貍離鯉禮麗厲勵礫歷瀝隸儷酈壢藶蒞蘺嚦邐驪縭櫪櫟轢礪鋰鸝癘糲躒靂鱺鱧倆聯蓮連鐮憐漣簾斂臉鏈戀煉練蘞奩瀲璉殮褳襝鰱糧涼兩輛諒魎療遼鐐繚釕鷯獵臨鄰鱗凜賃藺廩檁轔躪齡鈴靈嶺領綾欞蟶鯪餾劉瀏騮綹鎦鷚龍聾嚨籠壟攏隴蘢瀧瓏櫳朧礱樓婁摟簍僂蔞嘍嶁鏤瘺耬螻髏蘆盧顱廬爐擄鹵虜魯賂祿錄陸壚擼嚕閭瀘淥櫨櫓轤輅轆氌臚鸕鷺艫鱸巒攣孿灤亂臠孌欒鸞鑾掄輪倫侖淪綸論圇蘿羅邏鑼籮騾駱絡犖玀濼欏腡鏍驢呂鋁侶屢縷慮濾綠櫚褸鋝嘸媽瑪碼螞馬罵嗎嘜嬤榪買麥賣邁脈勱瞞饅蠻滿謾縵鏝顙鰻貓錨鉚貿麼沒鎂門悶們捫燜懣鍆錳夢瞇謎彌覓冪羋謐獼禰綿緬澠靦黽廟緲繆滅憫閩閔緡鳴銘謬謨驀饃歿鏌謀畝鉬吶鈉納難撓腦惱鬧鐃訥餒內擬膩鈮鯢攆輦鯰釀鳥蔦裊聶嚙鑷鎳隉蘗囁顢躡檸獰寧擰濘苧嚀聹鈕紐膿濃農儂噥駑釹諾儺瘧歐鷗毆嘔漚謳慪甌盤蹣龐拋皰賠轡噴鵬紕羆鈹騙諞駢飄縹頻貧嬪蘋憑評潑頗釙撲鋪樸譜鏷鐠棲臍齊騎豈啟氣棄訖蘄騏綺榿磧頎頏鰭牽釬鉛遷簽謙錢鉗潛淺譴塹僉蕁慳騫繾槧鈐槍嗆墻薔強搶嬙檣戧熗錆鏘鏹羥蹌鍬橋喬僑翹竅誚譙蕎繰磽蹺竊愜鍥篋欽親寢鋟輕氫傾頃請慶撳鯖瓊窮煢蛺巰賕蟣鰍趨區軀驅齲詘嶇闃覷鴝顴權勸詮綣輇銓卻鵲確闋闕愨讓饒擾繞蕘嬈橈熱韌認紉飪軔榮絨嶸蠑縟銣顰軟銳蜆閏潤灑薩颯鰓賽傘毿糝喪騷掃繅澀嗇銫穡殺剎紗鎩鯊篩曬釃刪閃陜贍繕訕姍騸釤鱔墑傷賞坰殤觴燒紹賒攝懾設厙灄畬紳審嬸腎滲詵諗瀋聲繩勝師獅濕詩時蝕實識駛勢適釋飾視試謚塒蒔弒軾貰鈰鰣壽獸綬樞輸書贖屬術樹豎數攄紓帥閂雙誰稅順說碩爍鑠絲飼廝駟緦鍶鷥聳慫頌訟誦擻藪餿颼鎪蘇訴肅謖穌雖隨綏歲誶孫損筍蓀猻縮瑣鎖嗩脧獺撻闥鉈鰨臺態鈦鮐攤貪癱灘壇譚談嘆曇鉭錟頇湯燙儻餳鐋鏜濤絳討韜鋱騰謄銻題體屜緹鵜闐條糶齠鰷貼鐵廳聽烴銅統慟頭鈄禿圖釷團摶頹蛻飩脫鴕馱駝橢籜鼉襪媧膃彎灣頑萬紈綰網輞韋違圍為濰維葦偉偽緯謂衛諉幃闈溈潿瑋韙煒鮪溫聞紋穩問閿甕撾蝸渦窩臥萵齷嗚鎢烏誣無蕪吳塢霧務誤鄔廡憮嫵騖鵡鶩錫犧襲習銑戲細餼鬩璽覡蝦轄峽俠狹廈嚇硤鮮纖賢銜閑顯險現獻縣餡羨憲線莧薟蘚峴獫嫻鷴癇蠔秈躚廂鑲鄉詳響項薌餉驤緗饗蕭囂銷曉嘯嘵瀟驍綃梟簫協挾攜脅諧寫瀉謝褻擷紲纈鋅釁興陘滎兇洶銹繡饈鵂虛噓須許敘緒續詡頊軒懸選癬絢諼鉉鏇學謔澩鱈勛詢尋馴訓訊遜塤潯鱘壓鴉鴨啞亞訝埡婭椏氬閹煙鹽嚴巖顏閻艷厭硯彥諺驗厴贗儼兗讞懨閆釅魘饜鼴鴦楊揚瘍陽癢養樣煬瑤搖堯遙窯謠藥軺鷂鰩爺頁業葉靨謁鄴曄燁醫銥頤遺儀蟻藝億憶義詣議誼譯異繹詒囈嶧飴懌驛縊軼貽釔鎰鐿瘞艤蔭陰銀飲隱銦癮櫻嬰鷹應纓瑩螢營熒蠅贏穎塋鶯縈鎣攖嚶瀅瀠瓔鸚癭頦罌喲擁傭癰踴詠鏞優憂郵鈾猶誘蕕銪魷輿魚漁娛與嶼語獄譽預馭傴俁諛諭蕷崳飫閾嫗紆覦歟鈺鵒鷸齬鴛淵轅園員圓緣遠櫞鳶黿約躍鑰粵悅閱鉞鄖勻隕運蘊醞暈韻鄆蕓惲慍紜韞殞氳雜災載攢暫贊瓚趲鏨贓臟駔鑿棗責擇則澤賾嘖幘簀賊譖贈綜繒軋鍘閘柵詐齋債氈盞斬輾嶄棧戰綻譫張漲帳賬脹趙詔釗蟄轍鍺這謫輒鷓貞針偵診鎮陣湞縝楨軫賑禎鴆掙睜猙爭幀癥鄭證諍崢鉦錚箏織職執紙摯擲幟質滯騭櫛梔軹輊贄鷙螄縶躓躑觶鐘終種腫眾鍾謅軸皺晝驟紂縐豬諸誅燭矚囑貯鑄駐佇櫧銖專磚轉賺囀饌顳樁莊裝妝壯狀錐贅墜綴騅縋諄準著濁諑鐲茲資漬諮緇輜貲眥錙齜鯔蹤總縱傯鄒諏騶鯫詛組鏃鉆纘躦鱒翺並蔔沈醜澱叠鬥範幹臯矽櫃後夥稭傑訣誇裏淩麽黴撚淒扡聖屍擡塗窪餵汙鍁鹹蠍彜湧遊籲禦願嶽雲竈紮劄築於誌註雕訁譾郤猛氹阪壟堖垵墊檾蕒葤蓧蒓菇槁摣咤唚哢噝噅撅劈謔襆嶴脊仿僥獁麅餘餷饊饢楞怵懍爿漵灩混濫瀦淡寧糸絝緔瑉梘棬案橰櫫軲軤賫膁腖飈糊煆溜湣渺碸滾瞘鈈鉕鋣銱鋥鋶鐦鐧鍩鍀鍃錇鎄鎇鎿鐝鑥鑹鑔穭鶓鶥鸌癧屙瘂臒襇繈耮顬蟎麯鮁鮃鮎鯗鯝鯴鱝鯿鰠鰵鱅鞽韝齇';
	}

    /**
     * 字符串从简体变成繁体
     * @param string $str 待转换的简体字符
     * @return string
     */
    public static function traditionalized($str)
    {
        // 转换完成的字符串
        $traditionalized = '';
        // 简体字符串
        $simplifiedChar = self::simplifiedChar();
        // 繁体字符串
        $traditionalChar = self::traditionalChar();
        // 字符串长度
        $length = mb_strlen($str);
        // 遍历字符串
        for($i=0; $i<$length; $i++){
            // 当前字符
            $currentChar = mb_substr($str, $i, 1);
            // 查询当前字符是否在简体字符串中
            $strPos = mb_strpos($simplifiedChar, $currentChar);
            // 如果在简体字符串
            if(false !== $strPos){
                $traditionalized .= mb_substr($traditionalChar, $strPos, 1);
            }
            // 不在简体字符串内
            else{
                $traditionalized .= $currentChar;
            }
                
        }
        return $traditionalized;
    }
      
    /**
     * 字符串从繁体变成简体
     * @param string $str 待转换的繁体字符
     * @return string
     */
    public static function simplized($str){
        // 转换完成的字符串
        $simplized = '';
        // 简体字符串
        $simplifiedChar = self::simplifiedChar();
        // 繁体字符串
        $traditionalChar = self::traditionalChar();
        // 字符串长度
        $length = mb_strlen($str);
        // 遍历字符串
        for($i=0; $i<$length; $i++){
            // 当前字符
            $currentChar = mb_substr($str, $i, 1);
            // 查询当前字符是否在简体字符串中
            $strPos = mb_strpos($traditionalChar, $currentChar);
            // 如果在简体字符串
            if(false !== $strPos){
                $simplized .= mb_substr($simplifiedChar, $strPos, 1);
            }
            // 不在简体字符串内
            else{
                $simplized .= $currentChar;
            }
                
        }
        return $simplized;
    }

    /**
     * 对字符串进行PKCS7填充补位
     * @param string $str 需要进行填充补位操作的字符串
     * @param int $blockSize 补位块大小
     * @return string 补位后的字符串
     */
    public static function addPKCS7Padding($str, $blockSize = 32)
    {
        // 清除空字符串
        $str = trim($str);
        // 补位长度
        $amount_to_pad = $blockSize - (strlen($str) % $blockSize);
        // 补位字符
        $pad_chr = chr($amount_to_pad);
        // 开始补位
        $str .= str_repeat($pad_chr, $amount_to_pad);
        // 返回
        return $str;
    }

    /**
     * 对字符串进行PKCS7补位删除
     * @param string $str 需要进行补位删除操作的字符串
     * @param int $blockSize 补位块大小
     * @return string 删除填充补位后的字符串
     */
    public static function stripPKCS7Padding($str, $blockSize = 32)
    {
        // 补位长度
        $pad = ord(substr($str, -1));
        if ($pad < 1 || $pad > $blockSize) {
            $pad = 0;
        }
        return substr($str, 0, strlen($str) - $pad);
    }

    /**
     * 对字符串进行Zero填充补位
     * @param string $str 需要进行填充补位操作的字符串
     * @param int $blockSize 补位块大小
     * @return string 补位后的字符串
     */
    public static function addZeroPadding($str, $blockSize = 16)
    {
        // 清除空字符串
        $str = trim($str);
        // 补位长度
        $amount_to_pad = $blockSize - ((strlen($str) % $blockSize) ?: $blockSize);
        // 如果字符串长度为0则补位一个完整块
        if(strlen($str) == 0){
            $amount_to_pad = $blockSize;
        }
        // 开始补位
        $str .= str_repeat("\0", $amount_to_pad);
        // 返回
        return $str;
    }

    /**
     * 对字符串进行Zero补位删除
     * @param string $str 需要进行补位删除操作的字符串
     * @return string 删除填充补位后的字符串
     */
    public static function stripZeroPadding($str)
    {
        return rtrim($str, "\0");
    }

    /**
     * 限制字符串长度
     * @param string $string
     * @param int|null $length
     * @return string
     */
    public static function substrR($string, $length = null)
    {
        return self::substr($string, 0, $length);
    }

    /**
     * 获取指定长度的随机字母数字组合的字符串
     * @overwrite
     * @param int $length
     * @param int $type
     * @param string $addChars
     * @return string
     */
    public static function random($length = 6, $type = null, $addChars = ''): string
    {
        $str = '';
        switch ($type) {
            // 纯字母
            case 0:
                $chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' . $addChars;
                break;
            // 纯数字
            case 1:
                $chars = str_repeat('0123456789', 3);
                break;
            // 大写字母
            case 2:
                $chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' . $addChars;
                break;
            // 小写字母
            case 3:
                $chars = 'abcdefghijklmnopqrstuvwxyz' . $addChars;
                break;
            // 中文字符
            case 4:
                $chars = '们以我到他会作时要动国产的一是工就年阶义发成部民可出能方进在了不和有大这主中人上为来分生对于学下级地个用同行面说种过命度革而多子后自社加小机也经力线本电高量长党得实家定深法表着水理化争现所二起政三好十战无农使性前等反体合斗路图把结第里正新开论之物从当两些还天资事队批点育重其思与间内去因件日利相由压员气业代全组数果期导平各基或月毛然如应形想制心样干都向变关问比展那它最及外没看治提五解系林者米群头意只明四道马认次文通但条较克又公孔领军流入接席位情运器并飞原油放立题质指建区验活众很教决特此常石强极土少已根共直团统式转别造切九你取西持总料连任志观调七么山程百报更见必真保热委手改管处己将修支识病象几先老光专什六型具示复安带每东增则完风回南广劳轮科北打积车计给节做务被整联步类集号列温装即毫知轴研单色坚据速防史拉世设达尔场织历花受求传口断况采精金界品判参层止边清至万确究书';
                break;
            // 字母+数字+特殊符号
            case 5:
                $chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789#@$%&_-' . $addChars;
                break;
            // 验证码
            case 6:
                $chars = 'ABCDEFGHIJKMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz23456789' . $addChars;
                break;
            // 字母+数字
            default:
                $chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789' . $addChars;
                break;
        }
        if ($length > 10) {
            $chars = $type == 1 ? str_repeat($chars, $length) : str_repeat($chars, 5);
        }
        if ($type != 4) {
            $chars = str_shuffle($chars);
            $str = substr($chars, 0, $length);
        } else {
            for ($i = 0; $i < $length; $i++) {
                $str .= mb_substr($chars, floor(mt_rand(0, mb_strlen($chars, 'utf-8') - 1)), 1);
            }
        }
        return $str;
    }

    /**
     * 生成UUID
     * @param bool $upper
     * @return string
     */
    public static function uuid($upper = false): string
    {
        // 函数随机数播种
        mt_srand((double) microtime() * 10000);
        // 生成随机字符串
        $charid = md5(uniqid(rand(), true));
        if(true === $upper){
            $charid = strtoupper($charid);
        }
        // 分隔符'-'
        $hyphen = chr(45);
        // 构造UUID
        $uuid = implode($hyphen, [
            substr($charid, 0, 8),
            substr($charid, 8, 4),
            substr($charid,12, 4),
            substr($charid,16, 4),
            substr($charid,20,12),
        ]);
        // 追加括号
        // $uuid = chr(123) . $uuid . chr(125);

        return $uuid;
    }

    /**
     * URL解码
     * @param string $value
     * @return string
     */
    public static function urldecode($value): string
    {
        // 如果为空
        if(empty($value)){
            return '';
        }

        try{
            // url解码
            $decodeValue = urldecode($value);
            // 获取编码类型
            $encodeType = mb_detect_encoding($decodeValue, ['UTF-8', 'GBK']);
            // 不是UTF-8编码
            if($encodeType != 'UTF-8'){
                // 编码转换为UTF-8
                $decodeValue = urlencode(iconv($encodeType, 'UTF-8', $decodeValue));
            }
        } catch (\Exception $e){
            $decodeValue = '';
        }
        // 返回
        return $decodeValue;
    }

    /**
     * 获取内容中的网址
     * @param string $content
     * @param bool $withQuery
     * @return string
     */
    public static function parseLink($content, $withQuery = false): string
    {
        // 匹配到的网址
        $linkMatched = [];
        // 开始匹配
        preg_match_all('#(http(s)?:\/\/)[a-zA-Z0-9\-\_\.\?\#\=\&\/\%]+#', $content, $linkMatched);
        // 如果没有匹配到
        if(!isset($linkMatched[0][0])){
            return '';
        }

        // 获取匹配到的第一个网址
        $link = $linkMatched[0][0];

        // 如果不需要保留参数
        if(!$withQuery){
            // 分隔问号
            $linkArray = explode('?', $link);
            // 分隔井号
            $linkArray = explode('#', $linkArray[0]);
            // 获取结果
            $link = $linkArray[0];
        }
        // 返回
        return $link;
    }
}
