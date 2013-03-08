"""
A simple web server, providing a restful API for 1KB data blocks.
"""
import tornado.web
import tornado.wsgi
import tornado.httpserver
import tornado.ioloop
import re, os

kBlockSize = 1024
rexVolume = '[a-zA-Z0-9-]+'

## the block device ############################################

class BlockDevice(object):
    """
    This simulates a small block-based storage device, with direct access
    to 65536 1-kilobyte data blocks. The files are stored in *.dat files,
    in a directory corresponding to the volume label.
    """
    root  = 'vol' # ex: ./vol/name/89AB.dat

    def __init__(self, volume):
        assert re.match(rexVolume, volume), 'invalid volume name'
        self.volume = volume

    def _path(self, n):
        """
        the blocks are sequenced in hexidecimal-numbered *.dat files.
        """
        return os.path.join(self.root, self.volume, '%04X.dat' % n)

    ## public interface ########################################

    def fetch(self, n):
        """Fetch data for block n, or None if block is empty."""
        path = self._path(n)
        if os.path.exists(path):
            with open(path, 'rb') as block:
                return bytes(block.read(kBlockSize))

    def store(self, n, data):
        """Store data for block n, adjusted to block size"""
        path = self._path(n)
        dirs = os.path.dirname(path)
        if not os.path.exists(dirs): os.makedirs(dirs)
        with open(path, 'wb') as block:
            # trim if too big, pad if too small:
            block.write(bytes(data[:kBlockSize]))
            block.write(bytes('\0' * (kBlockSize - block.tell())))

    def erase(self, n):
        """Permanently delete the data stored in block n."""
        path = self._path(n)
        if os.path.exists(path):
            os.unlink(path)

## general wsgi interface ######################################

def _head(mimetype, length=None):
    """Helper routine to build a WSGI/HTTP header"""
    result = [('Content-type', mimetype)]
    if length: result.append(('Content-Length', str(length)))
    return result

def generic(start, status, msg):
    """Helper routine to generate a generic html response page"""
    start('%i %s' % (status, msg), _head('text/html'))
    return ('<html><title>%i %s</title> <h1>%i %s</h1></html>'
            % (status, msg, status, msg))

def notfound(env, start):
    """WSGI app to show a 404 error"""
    return generic(start, 404, 'Not Found')

_cached = None
def homepage(env, start):
    """WSGI app to show the home page"""
    global _cached
    start('200 OK' , _head('text/html'))
    if not _cached: _cached = open('blockbox.html').read()
    return _cached

## wsgi interface for block device #############################

def get_block(env, start, device, blockn):
    """Handler for HTTP GET"""
    data = device.fetch(blockn)
    if data is None: return generic(start, 204, 'No Content')
    else:
        start('200 OK', _head('application/octet-stream', kBlockSize))
        # in 2.7, bytes==str and tornado will freak out, so:
        return data

def put_block(env, start, device, blockn):
    """Handler for HTTP PUT"""
    data = device.store(blockn, env['wsgi.input'].read())
    return generic(start, 205, 'Reset Content')

def del_block(env, start, device, blockn):
    """Handler for HTTP DELETE"""
    device.erase(blockn)
    return generic(start, 205, 'Reset Content')

kDispatch = {
    'GET' : get_block,
    'PUT' : put_block,
    'DELETE': del_block }
    
def blockapi(env, start):
    """WSGI app to serve the block device on the web"""
    _, _, volume, hexstr = env['PATH_INFO'].split('/')
    blockn = int(hexstr, 16)
    device = BlockDevice(volume)
    try: return kDispatch[
            env['REQUEST_METHOD']](env, start, device, blockn) 
    except KeyError:
        return generic(start, 405, 'Not Allowed')


## web server ##################################################

# The code above is compatible with any wsgi server.
# Everything below here is tornado-specific.
#
# I chose tornado as my server engine because it appears to
# have a decent websocket implementation, and I plan to add
# some collaboration features that make use of this soon.

def serve(pattern, app):
    """
    This is just a builder/macro to let us use tornado.web.Appplication
    for URL dispatch. It's just wrapping up each wsgi app (function)
    in the object structure tornado needs.
    """
    return (pattern, tornado.web.FallbackHandler,
            dict(fallback=tornado.wsgi.WSGIContainer(app)))

def main(host, port):
    """Serve the web interface on the specified host and port."""
    app = tornado.web.Application([
        serve(r'/$', homepage),
        serve(r'/v/%s/[a-f0-9]{4}$' % rexVolume, blockapi),
        # serve(r'/v/?$', volumes),
        # serve(r'/v/%s/?$' % rexVolume, volume),
        serve(r'.*', notfound),
    ], debug=False)

    server = tornado.httpserver.HTTPServer(app)
    server.listen(address=host, port=port)

    print("starting server at http://{0}:{1}/".format(host, port))
    tornado.ioloop.IOLoop.instance().start()

if __name__=="__main__":
    main(host='0.0.0.0', port='9876')
