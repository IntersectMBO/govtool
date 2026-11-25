import {
  Controller,
  Get,
  Headers,
  NotFoundException,
  Param,
  Res,
  UseGuards,
} from '@nestjs/common';
import type { Response } from 'express';

import { DirectOnlyGuard } from './direct-only.guard';
import { SeoService } from './seo.service';

const CARD_FILE = /^([0-9a-f]{12})\.png$/;

/**
 * The SEO surface. The frontend's nginx includes /seo/head and /seo/body
 * into every page (passing the page path in X-Seo-Path), and proxies its
 * /og/ and /sitemap.xml locations to /seo/image and /seo/sitemap.xml.
 */
@Controller('seo')
@UseGuards(DirectOnlyGuard)
export class SeoController {
  constructor(private readonly seo: SeoService) {}

  @Get('head')
  head(
    @Headers('x-seo-path') path: string | undefined,
    @Res({ passthrough: true }) response: Response,
  ): Promise<string> {
    response.type('text/html; charset=utf-8');
    response.setHeader('Cache-Control', 'no-store');
    return this.seo.head(path);
  }

  @Get('body')
  body(
    @Headers('x-seo-path') path: string | undefined,
    @Res({ passthrough: true }) response: Response,
  ): Promise<string> {
    response.type('text/html; charset=utf-8');
    response.setHeader('Cache-Control', 'no-store');
    return this.seo.body(path);
  }

  @Get('sitemap.xml')
  async sitemap(
    @Res({ passthrough: true }) response: Response,
  ): Promise<string> {
    const xml = await this.seo.sitemap();
    if (xml === null) throw new NotFoundException();
    response.type('application/xml; charset=utf-8');
    response.setHeader('Cache-Control', 'public, max-age=3600');
    return xml;
  }

  /** Handled by hand: a 301 and a binary body do not go through Nest's reply. */
  @Get('image/:kind/:id/:file')
  async image(
    @Param('kind') kind: string,
    @Param('id') id: string,
    @Param('file') file: string,
    @Res() response: Response,
  ): Promise<void> {
    const match = CARD_FILE.exec(file);
    const result =
      match === null ? null : await this.seo.image(kind, id, match[1]);
    if (result === null) throw new NotFoundException();
    if ('redirect' in result) {
      // Never cached: the current hash moves on when the card's text does,
      // and a cached redirect would pin an old URL to whatever came next.
      response.setHeader('Cache-Control', 'no-store');
      response.redirect(301, result.redirect);
      return;
    }
    response
      .status(200)
      .type('image/png')
      .setHeader('Cache-Control', 'public, max-age=31536000, immutable');
    response.send(result.png);
  }
}
