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

namespace axguowen\helper\crypt;

use axguowen\helper\Str;

/**
 * php mcrypt加解密小助手
 */
class Mcryptor
{
    /**
     * 字符串加密
     * @param string $source 要加密的字符串
     * @param string $key 加密密钥 16位字符串
     * @param string $iv 加密向量 16位字符串
     * @return string
     */
    public static function encrypt($source, $key, $iv = null)
    {
        // 如果加密向量为空
        if(is_null($iv)){
            // 获取密钥的MD5值
            $keyMd5 = hash('md5', $key);
            // 根据加密的密钥构造加密向量
            $iv = substr($keyMd5, 0, 8) . substr($keyMd5, -8);
        }
        // 根据补位类型进行补位
        $source = Str::addZeroPadding($source, 16);
        // 加密
        $encrypted = openssl_encrypt($source, 'AES-128-CBC', $key, OPENSSL_ZERO_PADDING, $iv);
        // 返回
        return $encrypted;
    }

    /**
     * 字符串解密
     * @param string $source 要解密的字符串
     * @param string $key 加密密钥 16位字符串
     * @param string $iv 加密向量 16位字符串
     * @return string
     */
    public static function decrypt($source, $key, $iv = null)
    {
        // 如果加密向量为空
        if(is_null($iv)){
            // 获取密钥的MD5值
            $keyMd5 = hash('md5', $key);
            // 根据加密的密钥构造加密向量
            $iv = substr($keyMd5, 0, 8) . substr($keyMd5, -8);
        }
        // 加密
        $decrypted = openssl_decrypt($source, 'AES-128-CBC', $key, OPENSSL_ZERO_PADDING, $iv);
        // 根据补位类型进行补位
        $decrypted = Str::stripZeroPadding($decrypted);
        // 返回
        return $decrypted;
    }
}
