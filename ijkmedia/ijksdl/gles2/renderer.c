/*
 * Copyright (c) 2016 Bilibili
 * copyright (c) 2016 Zhang Rui <bbcallen@gmail.com>
 *
 * This file is part of ijkPlayer.
 *
 * ijkPlayer is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * ijkPlayer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with ijkPlayer; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <pthread.h>
#include "internal.h"

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
bool activeEffect = false;

GLboolean IJK_GLES2_Render_SetupTextureWithPixelBuffer(IJK_GLES2_Renderer *renderer) {
    CFDictionaryRef empty = CFDictionaryCreate(kCFAllocatorDefault,
                                               NULL,
                                               NULL,
                                               0,
                                               &kCFTypeDictionaryKeyCallBacks,
                                               &kCFTypeDictionaryValueCallBacks);
    
    CFMutableDictionaryRef attrs = CFDictionaryCreateMutable(kCFAllocatorDefault,
                                                             1,
                                                             &kCFTypeDictionaryKeyCallBacks,
                                                             &kCFTypeDictionaryValueCallBacks);
    
    CFDictionarySetValue(attrs, kCVPixelBufferIOSurfacePropertiesKey, empty);
    
    CVReturn cvRet = CVPixelBufferCreate(kCFAllocatorDefault,
                                         renderer->frame_width,
                                         renderer->frame_height,
                                         kCVPixelFormatType_32BGRA,
                                         attrs,
                                         &renderer->cvBuffer);
    
    if (kCVReturnSuccess != cvRet) {
        ALOGE("[IJK_GLES2_Render_SetupTextureWithPixelBuffer] CVPixelBufferCreate error %d", cvRet);
    }
    
    cvRet = CVOpenGLESTextureCacheCreateTextureFromImage(kCFAllocatorDefault,
                                                         renderer->cvTextureCache,
                                                         renderer->cvBuffer,
                                                         NULL,
                                                         GL_TEXTURE_2D,
                                                         GL_RGBA,
                                                         renderer->frame_width,
                                                         renderer->frame_height,
                                                         GL_BGRA,
                                                         GL_UNSIGNED_BYTE,
                                                         0,
                                                         &renderer->cvTexture);
    
    CFRelease(attrs);
    CFRelease(empty);
    
    if (kCVReturnSuccess != cvRet) {
        ALOGE("[IJK_GLES2_Render_SetupTextureWithPixelBuffer] CVOpenGLESTextureCacheCreateTextureFromImage error %d", cvRet);
        return false;
    }
    
    renderer->frame_textures[0] = CVOpenGLESTextureGetName(renderer->cvTexture);
    glBindTexture(CVOpenGLESTextureGetTarget(renderer->cvTexture), renderer->frame_textures[0]);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);
    
    return true;
}

void IJK_GLES2_Renderer_InitResultTexture(IJK_GLES2_Renderer *renderer) {
    GLenum status;

    glGenFramebuffers(1, &renderer->frame_buffers[0]);
    glBindFramebuffer(GL_FRAMEBUFFER, renderer->frame_buffers[0]);
    
    IJK_GLES2_Render_SetupTextureWithPixelBuffer(renderer);

    glBindTexture(CVOpenGLESTextureGetTarget(renderer->cvTexture), renderer->frame_textures[0]);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, renderer->frame_textures[0], 0);
    
    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    glBindTexture(GL_TEXTURE_2D, 0);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void IJK_GLES2_Renderer_ReleaseResultTexture(IJK_GLES2_Renderer *renderer) {
    renderer->frame_textures[0] = 0;
    
    if (renderer->cvTexture) {
        CFRelease(renderer->cvTexture);
        renderer->cvTexture = nil;
    }

    if (renderer->cvBuffer) {
        CVPixelBufferRelease(renderer->cvBuffer);
        renderer->cvBuffer = nil;
    }

    if (renderer->frame_buffers[0]) {
        glDeleteFramebuffers(1, &renderer->frame_buffers[0]);
        renderer->frame_buffers[0] = 0;
    }
}

static void IJK_GLES2_printProgramInfo(GLuint program)
{
    if (!program)
        return;

    GLint info_len = 0;
    glGetProgramiv(program, GL_INFO_LOG_LENGTH, &info_len);
    if (!info_len) {
        ALOGE("[GLES2][Program] empty info\n");
        return;
    }

    char    buf_stack[32];
    char   *buf_heap = NULL;
    char   *buf      = buf_stack;
    GLsizei buf_len  = sizeof(buf_stack) - 1;
    if (info_len > sizeof(buf_stack)) {
        buf_heap = (char*) malloc(info_len + 1);
        if (buf_heap) {
            buf     = buf_heap;
            buf_len = info_len;
        }
    }

    glGetProgramInfoLog(program, buf_len, NULL, buf);
    ALOGE("[GLES2][Program] error %s\n", buf);

    if (buf_heap)
        free(buf_heap);
}

void IJK_GLES2_Renderer_reset(IJK_GLES2_Renderer *renderer)
{
    if (!renderer)
        return;

    if (renderer->vertex_shader)
        glDeleteShader(renderer->vertex_shader);
    if (renderer->fragment_shader)
        glDeleteShader(renderer->fragment_shader);
    if (renderer->program)
        glDeleteProgram(renderer->program);

    renderer->vertex_shader   = 0;
    renderer->fragment_shader = 0;
    renderer->program         = 0;

    for (int i = 0; i < IJK_GLES2_MAX_PLANE; ++i) {
        if (renderer->plane_textures[i]) {
            glDeleteTextures(1, &renderer->plane_textures[i]);
            renderer->plane_textures[i] = 0;
        }
    }
}

void IJK_GLES2_Renderer_free(IJK_GLES2_Renderer *renderer)
{
    if (!renderer)
        return;
    
    IJK_GLES2_Renderer_ReleaseResultTexture(renderer);
    
    if (renderer->cvTextureCache) {
        CFRelease(renderer->cvTextureCache);
        renderer->cvTextureCache = NULL;
    }

    if (renderer->func_destroy)
        renderer->func_destroy(renderer);

#if 0
    if (renderer->vertex_shader)    ALOGW("[GLES2] renderer: vertex_shader not deleted.\n");
    if (renderer->fragment_shader)  ALOGW("[GLES2] renderer: fragment_shader not deleted.\n");
    if (renderer->program)          ALOGW("[GLES2] renderer: program not deleted.\n");

    for (int i = 0; i < IJK_GLES2_MAX_PLANE; ++i) {
        if (renderer->plane_textures[i])
            ALOGW("[GLES2] renderer: plane texture[%d] not deleted.\n", i);
    }
#endif

    free(renderer);
}

void IJK_GLES2_Renderer_freeP(IJK_GLES2_Renderer **renderer)
{
    if (!renderer || !*renderer)
        return;

    IJK_GLES2_Renderer_free(*renderer);
    *renderer = NULL;
}

IJK_GLES2_Renderer *IJK_GLES2_Renderer_create_base(const char *fragment_shader_source)
{
    assert(fragment_shader_source);

    IJK_GLES2_Renderer *renderer = (IJK_GLES2_Renderer *)calloc(1, sizeof(IJK_GLES2_Renderer));
    if (!renderer)
        goto fail;

    renderer->vertex_shader = IJK_GLES2_loadShader(GL_VERTEX_SHADER, IJK_GLES2_getVertexShader_default());
    if (!renderer->vertex_shader)
        goto fail;

    renderer->fragment_shader = IJK_GLES2_loadShader(GL_FRAGMENT_SHADER, fragment_shader_source);
    if (!renderer->fragment_shader)
        goto fail;

    renderer->program = glCreateProgram();                          IJK_GLES2_checkError("glCreateProgram");
    if (!renderer->program)
        goto fail;

    glAttachShader(renderer->program, renderer->vertex_shader);     IJK_GLES2_checkError("glAttachShader(vertex)");
    glAttachShader(renderer->program, renderer->fragment_shader);   IJK_GLES2_checkError("glAttachShader(fragment)");
    glLinkProgram(renderer->program);                               IJK_GLES2_checkError("glLinkProgram");
    GLint link_status = GL_FALSE;
    glGetProgramiv(renderer->program, GL_LINK_STATUS, &link_status);
    if (!link_status)
        goto fail;


    renderer->av4_position = glGetAttribLocation(renderer->program, "av4_Position");                IJK_GLES2_checkError_TRACE("glGetAttribLocation(av4_Position)");
    renderer->av2_texcoord = glGetAttribLocation(renderer->program, "av2_Texcoord");                IJK_GLES2_checkError_TRACE("glGetAttribLocation(av2_Texcoord)");
    renderer->um4_mvp      = glGetUniformLocation(renderer->program, "um4_ModelViewProjection");    IJK_GLES2_checkError_TRACE("glGetUniformLocation(um4_ModelViewProjection)");

    return renderer;

fail:

    if (renderer && renderer->program)
        IJK_GLES2_printProgramInfo(renderer->program);

    IJK_GLES2_Renderer_free(renderer);
    return NULL;
}


IJK_GLES2_Renderer *IJK_GLES2_Renderer_create(SDL_VoutOverlay *overlay, void* context)
{
    if (!overlay)
        return NULL;

    IJK_GLES2_printString("Version", GL_VERSION);
    IJK_GLES2_printString("Vendor", GL_VENDOR);
    IJK_GLES2_printString("Renderer", GL_RENDERER);
    IJK_GLES2_printString("Extensions", GL_EXTENSIONS);

    IJK_GLES2_Renderer *renderer = NULL;
    switch (overlay->format) {
        case SDL_FCC_RV16:      renderer = IJK_GLES2_Renderer_create_rgb565(); break;
        case SDL_FCC_RV24:      renderer = IJK_GLES2_Renderer_create_rgb888(); break;
        case SDL_FCC_RV32:      renderer = IJK_GLES2_Renderer_create_rgbx8888(); break;
#ifdef __APPLE__
        case SDL_FCC_NV12:      renderer = IJK_GLES2_Renderer_create_yuv420sp(); break;
        case SDL_FCC__VTB:      renderer = IJK_GLES2_Renderer_create_yuv420sp_vtb(overlay); break;
#endif
        case SDL_FCC_YV12:      renderer = IJK_GLES2_Renderer_create_yuv420p(); break;
        case SDL_FCC_I420:      renderer = IJK_GLES2_Renderer_create_yuv420p(); break;
        case SDL_FCC_I444P10LE: renderer = IJK_GLES2_Renderer_create_yuv444p10le(); break;
        default:
            ALOGE("[GLES2] unknown format %4s(%d)\n", (char *)&overlay->format, overlay->format);
            return NULL;
    }
    
    renderer->frame_buffers[0] = 0;
    CVReturn err = CVOpenGLESTextureCacheCreate(kCFAllocatorDefault, NULL, (CVEAGLContext*)context, NULL, &renderer->cvTextureCache);
    if (err) {
        ALOGE("[IJK_GLES2_Renderer_create] CVOpenGLESTextureCacheCreate %d", err);
    }
    renderer->func_onCreated = overlay->func_onCreated;
    renderer->func_onSizeChanged = overlay->func_onSizeChanged;
    renderer->func_onDrawFrame = overlay->func_onDrawFrame;

    renderer->format = overlay->format;
    return renderer;
}

GLboolean IJK_GLES2_Renderer_isValid(IJK_GLES2_Renderer *renderer)
{
    return renderer && renderer->program ? GL_TRUE : GL_FALSE;
}

GLboolean IJK_GLES2_Renderer_isFormat(IJK_GLES2_Renderer *renderer, int format)
{
    if (!IJK_GLES2_Renderer_isValid(renderer))
        return GL_FALSE;

    return renderer->format == format ? GL_TRUE : GL_FALSE;
}

/*
 * Per-Context routine
 */
GLboolean IJK_GLES2_Renderer_setupGLES()
{
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);       IJK_GLES2_checkError_TRACE("glClearColor");
    glEnable(GL_CULL_FACE);                     IJK_GLES2_checkError_TRACE("glEnable(GL_CULL_FACE)");
    glCullFace(GL_BACK);                        IJK_GLES2_checkError_TRACE("glCullFace");
    glDisable(GL_DEPTH_TEST);

    return GL_TRUE;
}

static void IJK_GLES2_Renderer_Vertices_reset(IJK_GLES2_Renderer *renderer)
{
    renderer->vertices[0] = -1.0f;
    renderer->vertices[1] = -1.0f;
    renderer->vertices[2] =  1.0f;
    renderer->vertices[3] = -1.0f;
    renderer->vertices[4] = -1.0f;
    renderer->vertices[5] =  1.0f;
    renderer->vertices[6] =  1.0f;
    renderer->vertices[7] =  1.0f;
}

static void IJK_GLES2_Renderer_Vertices_apply(IJK_GLES2_Renderer *renderer)
{
    switch (renderer->gravity) {
        case IJK_GLES2_GRAVITY_RESIZE_ASPECT:
            break;
        case IJK_GLES2_GRAVITY_RESIZE_ASPECT_FILL:
            break;
        case IJK_GLES2_GRAVITY_RESIZE:
            IJK_GLES2_Renderer_Vertices_reset(renderer);
            return;
        default:
            ALOGE("[GLES2] unknown gravity %d\n", renderer->gravity);
            IJK_GLES2_Renderer_Vertices_reset(renderer);
            return;
    }

    if (renderer->layer_width <= 0 ||
        renderer->layer_height <= 0 ||
        renderer->frame_width <= 0 ||
        renderer->frame_height <= 0)
    {
        ALOGE("[GLES2] invalid width/height for gravity aspect\n");
        IJK_GLES2_Renderer_Vertices_reset(renderer);
        return;
    }

    float width     = renderer->frame_width;
    float height    = renderer->frame_height;

    if (renderer->frame_sar_num > 0 && renderer->frame_sar_den > 0) {
        width = width * renderer->frame_sar_num / renderer->frame_sar_den;
    }

    const float dW  = (float)renderer->layer_width	/ width;
    const float dH  = (float)renderer->layer_height / height;
    float dd        = 1.0f;
    float nW        = 1.0f;
    float nH        = 1.0f;

    switch (renderer->gravity) {
        case IJK_GLES2_GRAVITY_RESIZE_ASPECT_FILL:  dd = FFMAX(dW, dH); break;
        case IJK_GLES2_GRAVITY_RESIZE_ASPECT:       dd = FFMIN(dW, dH); break;
    }

    nW = (width  * dd / (float)renderer->layer_width);
    nH = (height * dd / (float)renderer->layer_height);

    renderer->vertices[0] = - nW;
    renderer->vertices[1] = - nH;
    renderer->vertices[2] =   nW;
    renderer->vertices[3] = - nH;
    renderer->vertices[4] = - nW;
    renderer->vertices[5] =   nH;
    renderer->vertices[6] =   nW;
    renderer->vertices[7] =   nH;
}

static void IJK_GLES2_Renderer_Vertices_reloadVertex(IJK_GLES2_Renderer *renderer)
{
    glVertexAttribPointer(renderer->av4_position, 2, GL_FLOAT, GL_FALSE, 0, renderer->vertices);    IJK_GLES2_checkError_TRACE("glVertexAttribPointer(av2_texcoord)");
    glEnableVertexAttribArray(renderer->av4_position);                                      IJK_GLES2_checkError_TRACE("glEnableVertexAttribArray(av2_texcoord)");
}

#define IJK_GLES2_GRAVITY_MIN                   (0)
#define IJK_GLES2_GRAVITY_RESIZE                (0) // Stretch to fill layer bounds.
#define IJK_GLES2_GRAVITY_RESIZE_ASPECT         (1) // Preserve aspect ratio; fit within layer bounds.
#define IJK_GLES2_GRAVITY_RESIZE_ASPECT_FILL    (2) // Preserve aspect ratio; fill layer bounds.
#define IJK_GLES2_GRAVITY_MAX                   (2)

GLboolean IJK_GLES2_Renderer_setGravity(IJK_GLES2_Renderer *renderer, int gravity, GLsizei layer_width, GLsizei layer_height)
{
    if (renderer->gravity != gravity && gravity >= IJK_GLES2_GRAVITY_MIN && gravity <= IJK_GLES2_GRAVITY_MAX)
        renderer->vertices_changed = 1;
    else if (renderer->layer_width != layer_width)
        renderer->vertices_changed = 1;
    else if (renderer->layer_height != layer_height)
        renderer->vertices_changed = 1;
    else
        return GL_TRUE;

    renderer->gravity      = gravity;
    renderer->layer_width  = layer_width;
    renderer->layer_height = layer_height;
    return GL_TRUE;
}

static void IJK_GLES2_Renderer_TexCoords_reset(IJK_GLES2_Renderer *renderer, bool flip)
{
    if (flip) {
        renderer->texcoords[0] = 0.0f;
        renderer->texcoords[1] = 0.0f;
        renderer->texcoords[2] = 1.0f;
        renderer->texcoords[3] = 0.0f;
        renderer->texcoords[4] = 0.0f;
        renderer->texcoords[5] = 1.0f;
        renderer->texcoords[6] = 1.0f;
        renderer->texcoords[7] = 1.0f;
    } else {
        renderer->texcoords[0] = 0.0f;
        renderer->texcoords[1] = 1.0f;
        renderer->texcoords[2] = 1.0f;
        renderer->texcoords[3] = 1.0f;
        renderer->texcoords[4] = 0.0f;
        renderer->texcoords[5] = 0.0f;
        renderer->texcoords[6] = 1.0f;
        renderer->texcoords[7] = 0.0f;
    }
}

static void IJK_GLES2_Renderer_TexCoords_cropRight(IJK_GLES2_Renderer *renderer, GLfloat cropRight)
{
    ALOGE("IJK_GLES2_Renderer_TexCoords_cropRight\n");
    renderer->texcoords[0] = 0.0f;
    renderer->texcoords[1] = 1.0f;
    renderer->texcoords[2] = 1.0f - cropRight;
    renderer->texcoords[3] = 1.0f;
    renderer->texcoords[4] = 0.0f;
    renderer->texcoords[5] = 0.0f;
    renderer->texcoords[6] = 1.0f - cropRight;
    renderer->texcoords[7] = 0.0f;
}

static void IJK_GLES2_Renderer_TexCoords_reloadVertex(IJK_GLES2_Renderer *renderer)
{
    glVertexAttribPointer(renderer->av2_texcoord, 2, GL_FLOAT, GL_FALSE, 0, renderer->texcoords);   IJK_GLES2_checkError_TRACE("glVertexAttribPointer(av2_texcoord)");
    glEnableVertexAttribArray(renderer->av2_texcoord);                                              IJK_GLES2_checkError_TRACE("glEnableVertexAttribArray(av2_texcoord)");
}

/*
 * Per-Renderer routine
 */
GLboolean IJK_GLES2_Renderer_use(IJK_GLES2_Renderer *renderer)
{
    if (!renderer)
        return GL_FALSE;

    assert(renderer->func_use);
    if (!renderer->func_use(renderer))
        return GL_FALSE;

    IJK_GLES_Matrix modelViewProj;
    IJK_GLES2_loadOrtho(&modelViewProj, -1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f);
    glUniformMatrix4fv(renderer->um4_mvp, 1, GL_FALSE, modelViewProj.m);                    IJK_GLES2_checkError_TRACE("glUniformMatrix4fv(um4_mvp)");

    IJK_GLES2_Renderer_TexCoords_reset(renderer, false);
    IJK_GLES2_Renderer_TexCoords_reloadVertex(renderer);

    IJK_GLES2_Renderer_Vertices_reset(renderer);
    IJK_GLES2_Renderer_Vertices_reloadVertex(renderer);

    return GL_TRUE;
}

/*
 * Per-Frame routine
 */
GLboolean IJK_GLES2_Renderer_renderOverlay(IJK_GLES2_Renderer *renderer, SDL_VoutOverlay *overlay)
{
    pthread_mutex_lock(&lock);
    if (activeEffect &&
        !renderer->frame_buffers[0] &&
        renderer->frame_width > 0 &&
        renderer->frame_height > 0) {
        
        IJK_GLES2_Renderer_InitResultTexture(renderer);

        //呼叫傳遞進來的onCreated方法及onSizeChanged方法
        renderer->func_onCreated();
        renderer->func_onSizeChanged(renderer->frame_width, renderer->frame_height);
        ALOGE("create frame_buffers and textures %dx%d", renderer->frame_width, renderer->frame_height);
    }
    
    // 使用者設定了Filter，就掛載frameBuffer，否則就按照原來的流程直接渲染到螢幕上
    GLint bindFrameBuffer = 0;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &bindFrameBuffer);
    if (activeEffect && renderer->frame_buffers[0]) {
        glBindFramebuffer(GL_FRAMEBUFFER, renderer->frame_buffers[0]);
        
        /* 這句一定要加上，否則無法增加了Filter之後，啟用了其他GLProgram，無法渲染原始視訊到Texture上去了 */
        IJK_GLES2_Renderer_use(renderer);
    }
    
    if (!renderer || !renderer->func_uploadTexture) {
        pthread_mutex_unlock(&lock);
        return GL_FALSE;
    }

    glClear(GL_COLOR_BUFFER_BIT);               IJK_GLES2_checkError_TRACE("glClear");

    GLsizei visible_width  = renderer->frame_width;
    GLsizei visible_height = renderer->frame_height;
    if (overlay) {
        visible_width  = overlay->w;
        visible_height = overlay->h;
        if (renderer->frame_width   != visible_width    ||
            renderer->frame_height  != visible_height   ||
            renderer->frame_sar_num != overlay->sar_num ||
            renderer->frame_sar_den != overlay->sar_den) {
            
            if (activeEffect) {
                IJK_GLES2_Renderer_ReleaseResultTexture(renderer);
            }

            renderer->frame_width   = visible_width;
            renderer->frame_height  = visible_height;
            renderer->frame_sar_num = overlay->sar_num;
            renderer->frame_sar_den = overlay->sar_den;

            renderer->vertices_changed = 1;
            
            if (activeEffect) {
                IJK_GLES2_Renderer_InitResultTexture(renderer);
                renderer->func_onSizeChanged(renderer->frame_width,renderer->frame_height);
                
                ALOGE("create frame_buffers and textures %dx%d", renderer->frame_width, renderer->frame_height);
                /* 這句一定要加上，否則無法增加了Filter之後，啟用了其他GLProgram，無法渲染原始視訊到Texture上去了 */
                IJK_GLES2_Renderer_use(renderer);
                
                glBindFramebuffer(GL_FRAMEBUFFER, renderer->frame_buffers[0]);
            }
        }
        
        if (activeEffect) {
            glGetIntegerv(GL_VIEWPORT, &renderer->previousViewport);
            glViewport(0, 0, renderer->frame_width, renderer->frame_height);
        }

        renderer->last_buffer_width = renderer->func_getBufferWidth(renderer, overlay);

        if (!renderer->func_uploadTexture(renderer, overlay))
            return GL_FALSE;
    } else {
        // NULL overlay means force reload vertice
        renderer->vertices_changed = 1;
    }
    
    if (renderer->vertices_changed || activeEffect) {
        renderer->vertices_changed = 0;
        IJK_GLES2_Renderer_Vertices_apply(renderer);
        IJK_GLES2_Renderer_Vertices_reloadVertex(renderer);
    }

    GLsizei buffer_width = renderer->last_buffer_width;
    if (renderer->vertices_changed ||
        (buffer_width > 0 &&
         buffer_width > visible_width &&
         buffer_width != renderer->buffer_width &&
         visible_width != renderer->visible_width)){

        renderer->vertices_changed = 0;

        IJK_GLES2_Renderer_Vertices_apply(renderer);
        IJK_GLES2_Renderer_Vertices_reloadVertex(renderer);

        renderer->buffer_width  = buffer_width;
        renderer->visible_width = visible_width;

        GLsizei padding_pixels     = buffer_width - visible_width;
        GLfloat padding_normalized = ((GLfloat)padding_pixels) / buffer_width;

        IJK_GLES2_Renderer_TexCoords_reset(renderer, false);
        IJK_GLES2_Renderer_TexCoords_cropRight(renderer, padding_normalized);
        IJK_GLES2_Renderer_TexCoords_reloadVertex(renderer);
    }

    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);      IJK_GLES2_checkError_TRACE("glDrawArrays");
    
    // 使用者設定了Filter，就取消掛載FrameBuffer,並呼叫Filter的onDrawFrame方法。
    if (activeEffect && renderer->frame_buffers[0]) {
        glBindFramebuffer(GL_FRAMEBUFFER, bindFrameBuffer);
        glUniform1f(renderer->rgbTextureLocation, 1.0);
        glUniform1i(renderer->us2_sampler[2], 2);
        IJK_GLES2_Renderer_TexCoords_reset(renderer, true);
        IJK_GLES2_Renderer_TexCoords_reloadVertex(renderer);
        renderer->func_onDrawFrame(renderer->cvBuffer);
    }
    
    pthread_mutex_unlock(&lock);

    return GL_TRUE;
}

void IJK_GLES2_Renderer_drawEffect(IJK_GLES2_Renderer *renderer, GLuint textureId) {
    IJK_GLES2_Renderer_use(renderer);
    glViewport(renderer->previousViewport[0], renderer->previousViewport[1], renderer->previousViewport[2], renderer->previousViewport[3]);
    glActiveTexture(GL_TEXTURE0 + 2);
    glBindTexture(GL_TEXTURE_2D, textureId);
    
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);      IJK_GLES2_checkError_TRACE("glDrawArrays");
}

void IJK_GLES2_Renderer_activeEffect(bool active) {
    pthread_mutex_lock(&lock);
    activeEffect = active;
    pthread_mutex_unlock(&lock);
}
