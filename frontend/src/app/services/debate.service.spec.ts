import { TestBed } from '@angular/core/testing';

import { DebateService } from './debate.service';

describe('DebateService', () => {
  beforeEach(() => TestBed.configureTestingModule({}));

  it('should be created', () => {
    const service: DebateService = TestBed.get(DebateService);
    expect(service).toBeTruthy();
  });
});
