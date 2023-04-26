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
 * Xdb搜索器
 */
class XdbSearcher
{
    const HeaderInfoLength = 256;
    const VectorIndexRows = 256;
    const VectorIndexCols = 256;
    const VectorIndexSize = 8;
    const SegmentIndexSize = 14;

    /**
     * xdb文件句柄
     * @var string
     */
    protected $handle = null;

    /**
     * 头部信息
     * @var array
     */
    protected $header = null;

    /**
     * IO次数统计
     * @var int
     */
    protected $ioCount = 0;

    /**
     * 二进制字符串中的矢量索引
     * 字符串解码将比基于映射的数组更快
     * @var int
     */
    protected $vectorIndex = null;

    /**
     * xdb文件内容缓冲
     * @var string
     */
    protected $contentBuff = null;

    /**
     * 创建基于xdb文件的搜索器
     * @access public
     * @param string $dbFile xdb文件路径
     * @return static
     */
    public static function newWithFileOnly($dbFile)
    {
        return new static($dbFile, null, null);
    }

    /**
     * 创建基于vectorIndex缓存的搜索器
     * @access public
     * @param string $dbFile xdb文件路径
     * @param int $vIndex 索引
     * @return static
     */
    public static function newWithVectorIndex($dbFile, $vIndex)
    {
        return new static($dbFile, $vIndex);
    }

    /**
     * 创建基于xdb内容缓冲的搜索器
     * @access public
     * @param string $cBuff xdb文件内容缓冲
     * @return static
     */
    public static function newWithBuffer($cBuff)
    {
        return new static(null, null, $cBuff);
    }

    /**
     * 构建方法
     * @access public
     * @param string $dbFile xdb文件路径
     * @param int $vectorIndex 索引
     * @param string $cBuff xdb文件内容缓冲
     * @return void
     * @throws \Exception
     */
    public function __construct($dbFile = null, $vectorIndex = null, $cBuff = null)
    {
        // check the content buffer first
        if ($cBuff != null) {
            $this->vectorIndex = null;
            $this->contentBuff = $cBuff;
        } else {
            // 加载默认数据文件 by Anyon
            if (is_null($dbFile)) {
                $dbFile = __DIR__ . DIRECTORY_SEPARATOR . 'ip2region.xdb';
            }
            // open the xdb binary file
            $this->handle = fopen($dbFile, "r");
            if ($this->handle === false) {
                throw new \Exception("failed to open xdb file '%s'", $dbFile);
            }

            $this->vectorIndex = $vectorIndex;
        }
    }

    /**
     * 关闭文件句柄
     * @access public
     * @return void
     */
    public function close()
    {
        if ($this->handle != null) {
            fclose($this->handle);
        }
    }

    /**
     * 获取IO次数
     * @access public
     * @return int
     */
    public function getIOCount()
    {
        return $this->ioCount;
    }

    /**
     * 查询IP
     * @access public
     * @param string $ip IP地址
     * @return string
     * @throws \Exception
     */
    public function search($ip)
    {
        // 校验并转换IP地址
        if (is_string($ip)) {
            $t = self::ip2long($ip);
            if ($t === null) {
                throw new \Exception('invalid ip address ' . $ip);
            }
            $ip = $t;
        }

        // 重置IO计数
        $this->ioCount = 0;

        // 基于矢量索引定位分段索引块
        $il0 = ($ip >> 24) & 0xFF;
        $il1 = ($ip >> 16) & 0xFF;
        $idx = $il0 * self::VectorIndexCols * self::VectorIndexSize + $il1 * self::VectorIndexSize;
        if ($this->vectorIndex != null) {
            $sPtr = self::getLong($this->vectorIndex, $idx);
            $ePtr = self::getLong($this->vectorIndex, $idx + 4);
        }
        elseif ($this->contentBuff != null) {
            $sPtr = self::getLong($this->contentBuff, self::HeaderInfoLength + $idx);
            $ePtr = self::getLong($this->contentBuff, self::HeaderInfoLength + $idx + 4);
        }
        else {
            // 读取矢量索引块
            $buff = $this->read(self::HeaderInfoLength + $idx, 8);
            if ($buff === null) {
                throw new \Exception('failed to read vector index at ' . $idx);
            }

            $sPtr = self::getLong($buff, 0);
            $ePtr = self::getLong($buff, 4);
        }

        // printf("sPtr: %d, ePtr: %d\n", $sPtr, $ePtr);

        // 二进制搜索段索引以获取区域信息
        $dataLen = 0;
        $dataPtr = null;
        $l = 0;
        $h = ($ePtr - $sPtr) / self::SegmentIndexSize;
        while ($l <= $h) {
            $m = ($l + $h) >> 1;
            $p = $sPtr + $m * self::SegmentIndexSize;

            // 读取索引段
            $buff = $this->read($p, self::SegmentIndexSize);
            if ($buff == null) {
                throw new \Exception('failed to read segment index at ' . $p);
            }

            $sip = self::getLong($buff, 0);
            if ($ip < $sip) {
                $h = $m - 1;
            } else {
                $eip = self::getLong($buff, 4);
                if ($ip > $eip) {
                    $l = $m + 1;
                } else {
                    $dataLen = self::getShort($buff, 8);
                    $dataPtr = self::getLong($buff, 10);
                    break;
                }
            }
        }

        // 未匹配到任何数据
        // printf("dataLen: %d, dataPtr: %d\n", $dataLen, $dataPtr);
        if ($dataPtr == null) {
            return null;
        }

        // 加载并返回区域数据
        $buff = $this->read($dataPtr, $dataLen);
        if ($buff == null) {
            return null;
        }

        return $buff;
    }

    /**
     * 从指定的索引中读取指定的字节
     * @access protected
     * @param int $offset 偏移量
     * @param int $len 长度
     * @return string
     */
    protected function read($offset, $len)
    {
        // 先检查内存中的缓冲区
        if ($this->contentBuff != null) {
            return substr($this->contentBuff, $offset, $len);
        }

        // read from the file
        $r = fseek($this->handle, $offset);
        if ($r == -1) {
            return null;
        }

        $this->ioCount++;
        $buff = fread($this->handle, $len);
        if ($buff === false) {
            return null;
        }

        if (strlen($buff) != $len) {
            return null;
        }

        return $buff;
    }

    // --- 静态工具方法 ----

    /**
     * 将字符串IP地址转换为地址值
     * @access public
     * @param string $ip IP地址
     * @return int
     */
    public static function ip2long($ip)
    {
        $ip = ip2long($ip);
        if ($ip === false) {
            return null;
        }

        // 如果在32位操作系统上，则将带符号的int转换为无符号的int
        if ($ip < 0 && PHP_INT_SIZE == 4) {
            $ip = sprintf("%u", $ip);
        }

        return $ip;
    }

    /**
     * 从字节缓冲区读取4个长字节
     * @access public
     * @param string $b 缓冲数据
     * @param string $idx 索引
     * @return string
     */
    public static function getLong($b, $idx)
    {
        $val = (ord($b[$idx])) | (ord($b[$idx + 1]) << 8)
            | (ord($b[$idx + 2]) << 16) | (ord($b[$idx + 3]) << 24);

        // convert signed int to unsigned int if on 32 bit operating system
        if ($val < 0 && PHP_INT_SIZE == 4) {
            $val = sprintf("%u", $val);
        }

        return $val;
    }

    /**
     * 从字节缓冲区中读取2个短字节
     * @access public
     * @param string $b 缓冲数据
     * @param int $idx 索引
     * @return string
     */
    public static function getShort($b, $idx)
    {
        return ((ord($b[$idx])) | (ord($b[$idx + 1]) << 8));
    }

    /**
     * 从指定的文件句柄加载头信息
     * @access public
     * @param string $handle 文件句柄
     * @return array
     */
    public static function loadHeader($handle)
    {
        if (fseek($handle, 0) == -1) {
            return null;
        }

        $buff = fread($handle, self::HeaderInfoLength);
        if ($buff === false) {
            return null;
        }

        // 读取字节长度检查
        if (strlen($buff) != self::HeaderInfoLength) {
            return null;
        }

        // 返回解码后的头信息
        return [
            'version'       => self::getShort($buff, 0),
            'indexPolicy'   => self::getShort($buff, 2),
            'createdAt'     => self::getLong($buff, 4),
            'startIndexPtr' => self::getLong($buff, 8),
            'endIndexPtr'   => self::getLong($buff, 12)
        ];
    }

    /**
     * 从指定的xdb文件路径加载标头信息
     * @access public
     * @param string $dbFile 文件路径
     * @return array
     */
    public static function loadHeaderFromFile($dbFile)
    {
        $handle = fopen($dbFile, 'r');
        if ($handle === false) {
            return null;
        }

        $header = self::loadHeader($handle);
        fclose($handle);
        return $header;
    }

    /**
     * 从文件句柄加载矢量索引
     * @access public
     * @param string $handle 文件句柄
     * @return string
     */
    public static function loadVectorIndex($handle)
    {
        if (fseek($handle, self::HeaderInfoLength) == -1) {
            return null;
        }

        $rLen = self::VectorIndexRows * self::VectorIndexCols * self::SegmentIndexSize;
        $buff = fread($handle, $rLen);
        if ($buff === false) {
            return null;
        }

        if (strlen($buff) != $rLen) {
            return null;
        }

        return $buff;
    }

    /**
     * 从指定的xdb文件路径加载矢量索引
     * @access public
     * @param string $dbFile 文件路径
     * @return string
     */
    public static function loadVectorIndexFromFile($dbFile)
    {
        $handle = fopen($dbFile, 'r');
        if ($handle === false) {
            return null;
        }

        $vIndex = self::loadVectorIndex($handle);
        fclose($handle);
        return $vIndex;
    }

    /**
     * 从文件句柄加载xdb内容
     * @access public
     * @param string $handle 文件句柄
     * @return string
     */
    public static function loadContent($handle)
    {
        if (fseek($handle, 0, SEEK_END) == -1) {
            return null;
        }

        $size = ftell($handle);
        if ($size === false) {
            return null;
        }

        // 读取头部
        if (fseek($handle, 0) == -1) {
            return null;
        }

        $buff = fread($handle, $size);
        if ($buff === false) {
            return null;
        }

        // 读取长度检查
        if (strlen($buff) != $size) {
            return null;
        }

        return $buff;
    }

    /**
     * 从文件路径加载xdb内容
     * @access public
     * @param string $dbFile 文件路径
     * @return string
     */
    public static function loadContentFromFile($dbFile)
    {
        $str = file_get_contents($dbFile, false);
        if ($str === false) {
            return null;
        } else {
            return $str;
        }
    }

    /**
     * 当前时间的微秒
     * @access public
     * @return int
     */
    public static function now()
    {
        return (microtime(true) * 1000);
    }

}