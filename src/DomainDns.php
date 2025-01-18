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

class DomainDns
{
    /**
     * 获取全部可访问的解析记录
     * @param string $domain
     * @param string $host
     * @return array
     */
    public static function getRecords(string $domain): array
    {
        try{
            // 获取全部记录
            $records = dns_get_record($domain, DNS_A | DNS_AAAA | DNS_CNAME);
        } catch (\Exception $e){
            return [null, $e];
        }
        // 如果失败或者为空
        if(!is_array($records)){
            return [null, new \Exception('获取域名解析信息失败')];
        }
        // 返回
        return [$records, null];
    }

    /**
     * 检查域名A记录是否解析到指定主机
     * @param string $domain
     * @param string $host
     * @return array
     */
    public static function isToA(string $domain, string $host): array
    {
        // 获取解析记录
        $getRecordsResult = static::getRecords($domain);
        // 如果失败
        if(is_null($getRecordsResult[0])){
            return [null, new \Exception('域名未解析，请将域名做A记录解析到 <' . $host . '>')];
        }
        // 获取正确的结果
        $records = $getRecordsResult[0];
        // 当前域名总记录数量
        $all_count = 0;
        // 当前域名正确的记录数量
        $right_count = 0;
        // 遍历解析记录
        foreach($records as $record){
            // 如果主机记录不一致
            if($record['host'] != $domain){
                continue;
            }
            // 如果没有记录
            if(!isset($record['target']) && !isset($record['ip'])){
                continue;
            }
            // 总记录数量自增
            $all_count++;
            // 如果解析正确
            if($record['type'] == 'A'){
                if((isset($record['target']) && $record['target'] == $host) || (isset($record['ip']) && $record['ip'] == $host)){
                    $right_count++;
                }
            }
        }
        // 如果总记录数为空
        if($all_count == 0){
            return [null, new \Exception('域名未解析，请将域名做A记录解析到 <' . $host . '>')];
        }
        // 如果正确记录数为0
        if($right_count == 0){
            return [null, new \Exception('域名解析错误，请将域名的解析记录更换为A记录并解析到 <' . $host . '>')];
        }
        // 如果正确的数量小于总数量
        if($right_count < $all_count){
            return [null, new \Exception('该域名前缀可能存在多个解析，请删除解析地址为 <' . $host . '> 之外的其它解析记录')];
        }
        // 返回正确
        return ['解析正确', null];
    }

    /**
     * 检查域名AAAA记录是否解析到指定主机
     * @param string $domain
     * @param string $host
     * @return array
     */
    public static function isToAaaa(string $domain, string $host): array
    {
        // 获取解析记录
        $getRecordsResult = static::getRecords($domain);
        // 如果失败
        if(is_null($getRecordsResult[0])){
            return [null, new \Exception('域名未解析，请将域名做AAAA记录解析到 <' . $host . '>')];
        }
        // 获取正确的结果
        $records = $getRecordsResult[0];
        // 当前域名总记录数量
        $all_count = 0;
        // 当前域名正确的记录数量
        $right_count = 0;
        // 遍历解析记录
        foreach($records as $record){
            // 如果主机记录不一致
            if($record['host'] != $domain){
                continue;
            }
            // 如果没有记录
            if(!isset($record['target'])){
                continue;
            }
            // 总记录数量自增
            $all_count++;
            // 如果解析正确
            if($record['type'] == 'AAAA' && $record['target'] == $host){
                $right_count++;
            }
        }
        // 如果总记录数为空
        if($all_count == 0){
            return [null, new \Exception('域名未解析，请将域名做AAAA记录解析到 <' . $host . '>')];
        }
        // 如果正确记录数为0
        if($right_count == 0){
            return [null, new \Exception('域名解析错误，请将域名的解析记录更换为AAAA记录并解析到 <' . $host . '>')];
        }
        // 如果正确的数量小于总数量
        if($right_count < $all_count){
            return [null, new \Exception('该域名前缀可能存在多个解析，请删除解析地址为 <' . $host . '> 之外的其它解析记录')];
        }
        // 返回正确
        return ['解析正确', null];
    }

    /**
     * 检查域名CNAME记录是否解析到指定主机
     * @param string $domain
     * @param string $host
     * @return array
     */
    public static function isToCname(string $domain, string $host): array
    {
        // 获取解析记录
        $getRecordsResult = static::getRecords($domain);
        // 如果失败
        if(is_null($getRecordsResult[0])){
            return [null, new \Exception('域名未解析，请将域名做CNAME记录解析到 <' . $host . '>')];
        }
        // 获取正确的结果
        $records = $getRecordsResult[0];
        // 当前域名总记录数量
        $all_count = 0;
        // 当前域名正确的记录数量
        $right_count = 0;
        // 遍历解析记录
        foreach($records as $record){
            // 如果主机记录不一致
            if($record['host'] != $domain){
                continue;
            }
            // 如果没有记录
            if(!isset($record['target'])){
                continue;
            }
            // 总记录数量自增
            $all_count++;
            // 如果解析正确
            if($record['type'] == 'CNAME' && $record['target'] == $host){
                $right_count++;
            }
        }
        // 如果总记录数为空
        if($all_count == 0){
            return [null, new \Exception('域名未解析，请将域名做CNAME记录解析到 <' . $host . '>')];
        }
        // 如果正确记录数为0
        if($right_count == 0){
            return [null, new \Exception('域名解析错误，请将域名的解析记录更换为CNAME记录并解析到 <' . $host . '>')];
        }
        // 如果正确的数量小于总数量
        if($right_count < $all_count){
            return [null, new \Exception('该域名前缀可能存在多个解析，请删除解析地址为 <' . $host . '> 之外的其它解析记录')];
        }
        // 返回正确
        return ['解析正确', null];
    }

    /**
     * 检查域名解析是否正确
     * @param string $domain
     * @param string $host
     * @return array
     */
    public static function isCorrect(string $domain, string $host): array
    {
        // 如果主机地址是IPV4
        if(false !== filter_var($host, FILTER_VALIDATE_IP, FILTER_FLAG_IPV4)){
            return static::isToA($domain, $host);
        }
        // 如果主机地址是IPV6
        if(false !== filter_var($host, FILTER_VALIDATE_IP, FILTER_FLAG_NO_RES_RANGE)){
            return  static::isToAaaa($domain, $host);
        }
        // 判断是否是CNAME解析
        return static::isToCname($domain, $host);
    }
}
