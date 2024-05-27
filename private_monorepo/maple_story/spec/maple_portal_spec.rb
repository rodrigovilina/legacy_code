# frozen_string_literal: true

require 'maple_portal'

RSpec.describe MaplePortal do
  let(:portal) { Class.new { include MaplePortal }.new }

  describe 'constants' do
    it 'defines TELEPORT_PORTAL' do
      expect(MaplePortal::TELEPORT_PORTAL).to eq(1)
    end

    it 'defines MAP_PORTAL' do
      expect(MaplePortal::MAP_PORTAL).to eq(2)
    end

    it 'defines DOOR_PORTAL' do
      expect(MaplePortal::DOOR_PORTAL).to eq(6)
    end

    it 'defines OPEN' do
      expect(MaplePortal::OPEN).to be(true)
    end

    it 'defines CLOSED' do
      expect(MaplePortal::CLOSED).to be(false)
    end
  end

  describe 'methods' do
    it 'raises NotImplementedError for #type' do
      expect { portal.type }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #id' do
      expect { portal.id }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #position' do
      expect { portal.position }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #name' do
      expect { portal.name }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #target' do
      expect { portal.target }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #script_name' do
      expect { portal.script_name }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #script_name=' do
      expect { portal.script_name = 'new_name' }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #set_portal_status' do
      expect { portal.portal_status = true }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #portal_status' do
      expect { portal.portal_status }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #target_map_id' do
      expect { portal.target_map_id }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #enter_portal' do
      expect { portal.enter_portal('client') }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #set_portal_state' do
      expect { portal.portal_state = 'state' }.to raise_error(NotImplementedError)
    end

    it 'raises NotImplementedError for #portal_state' do
      expect { portal.portal_state }.to raise_error(NotImplementedError)
    end
  end
end
